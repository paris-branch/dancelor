open Nes open Option.Syntax
open Dancelor_common
open Dancelor_server_controller
open Cohttp_lwt_unix
module Log = (val Dancelor_server_logs.create "main" : Logs.LOG)

type query = (string * string list) list
[@@deriving show]

let cleanup_query query =
  let rec remove_empties = function
    | [] -> []
    | (_, [""]) :: t -> remove_empties t
    | x :: t -> x :: remove_empties t
  in
  let rec merge_duplicates = function
    | [] -> []
    | [m] -> [m]
    | (k1, v1) :: (k2, v2) :: query when k1 = k2 ->
       merge_duplicates ((k1, v1 @ v2) :: query)
    | (k, v) :: query ->
       (k, v) :: merge_duplicates query
  in
  query
  |> remove_empties
  (* FIXME: Before sorting, shouldn't we rename key[] into key? *)
  |> List.sort (fun (k1, _) (k2, _) -> compare k1 k2)
  |> merge_duplicates

let log_exn ~msg exn =
  Log.err @@ fun m ->
  m "%a" (Format.pp_multiline_sensible msg)
    ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()))

let log_exit = Dancelor_server_logs.log_exit (module Log)
let log_die () = Dancelor_server_logs.log_die (module Log)

let remove_prefix_suffix prefix suffix string =
  Option.assert_ (String.starts_with ~needle:prefix string) >>=? fun () ->
  String.remove_prefix ~needle:prefix string >>=? fun string ->
  Option.assert_ (String.ends_with ~needle:suffix string) >>=? fun () ->
  String.remove_suffix ~needle:suffix string

type controller =
  | C : ('any Slug.t ->
         (string * Yojson.Safe.t) list ->
         (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
      -> controller

let apply_controller path =
  if path = "/victor"  then log_exit 101;
  if path = "/victor2" then log_exit 102;
  if path = "/victor3" then log_exit 103;
  if path = "/victor4" then log_exit 104;
  [ "/book/",       ".pdf", C Book.Pdf.get ;
    "/set/",        ".ly",  C Set.Ly.get ;
    "/set/",        ".pdf", C Set.Pdf.get ;
    "/version/",    ".ly",  C Version.get_ly ;
    "/version/",    ".svg", C Version.Svg.get ;
    "/version/",    ".ogg", C Version.Ogg.get ;
    "/version/",    ".pdf", C Version.Pdf.get ]
  |> List.map
    (fun (prefix, suffix, controller) ->
       remove_prefix_suffix prefix suffix path, controller)
  |> List.find
    (fun (slug, _) -> slug <> None)
  |> (fun (slug, C controller) ->
      controller (Slug.unsafe_of_string (Option.unwrap slug)))

let callback _ request body =
  (* We have a double try ... with to catch all non-Lwt and Lwt
     exceptions. *)
  try
    try%lwt
      let uri = Request.uri request in
      let meth = Request.meth request in
      let path = Uri.path uri in
      Log.info (fun m -> m "Request for %s" path);
      (* Get query, parse it as an associative JSON *)
      let query =
        List.map
          (fun (k, vs) ->
             (k,
              match vs with
              | [v] -> Yojson.Safe.from_string v
              | vs -> `List (List.map Yojson.Safe.from_string vs)))
          (Uri.query uri)
      in
      (* Get body, parse it as an associative JSON *)
      let%lwt body =
        let%lwt body = Cohttp_lwt.Body.to_string body in
        let body = if body = "" then "{}" else body in
        Log.debug (fun m -> m "Body: %s" body);
        let body = Yojson.Safe.from_string body in
        let body = match body with `Assoc body -> body | _ -> assert false in
        Lwt.return body
      in
      (* Consolidate query and body into a single query. Body takes precedence. *)
      let query = body @ query in
      let full_path = Filename.(concat (concat !Dancelor_server_config.share "static") path) in
      Log.debug (fun m -> m "Looking for %s" full_path);
      if Sys.file_exists full_path && not (Sys.is_directory full_path) then
        (
          Log.debug (fun m -> m "Serving static file.");
          Server.respond_file ~fname:full_path ()
        )
      else
        (
          Log.debug (fun m -> m "Asking Madge for %s." path);
          match%lwt Madge_server.handle meth path query with
          | Some response -> Lwt.return response
          | None ->
            if String.length path >= 5 && String.sub path 0 5 = "/"^Constant.api_prefix^"/" then
              (
                let path = String.sub path 4 (String.length path - 4) in
                Log.debug (fun m -> m "Looking for an API controller for %s." path);
                apply_controller path query
              )
            else
              (
                Log.debug (fun m -> m "Serving main file.");
                Server.respond_file ~fname:Filename.(concat (concat !Dancelor_server_config.share "static") "index.html") ()
              )
        )
    with
      exn ->
      log_exn ~msg:"Uncaught Lwt exception in the callback" exn;
      Server.respond_error ~status:`Internal_server_error ~body:"{}" ()
  with
    exn ->
    log_exn ~msg:"Uncaught exception in the callback" exn;
    Server.respond_error ~status:`Internal_server_error ~body:"{}" ()

let () =
  Lwt.async_exception_hook :=
    (function
      | Unix.Unix_error(Unix.EPIPE, _, _) ->
        Log.warn (fun m -> m "Connection closed by the client")
      | exn ->
        log_exn ~msg:"Uncaught asynchronous exception" exn)

let read_configuration () =
  Log.info (fun m -> m "Reading configuration");
  Dancelor_server_config.parse_cmd_line ()

let initialise_logs () =
  Dancelor_server_logs.initialise !Dancelor_server_config.loglevel

let initialise_database () =
  Log.info (fun m -> m "Initialising database");
  Dancelor_server_database.Tables.initialise ()

let check_init_only () =
  if !Dancelor_server_config.init_only then
    (
      Log.info (fun m -> m "Init only mode. Stopping now.");
      log_exit 0
    )

let start_routines () =
  if !Dancelor_server_config.routines then
    (
      Log.info (fun m -> m "Starting routines");
      Routine.initialise ()
    )
  else
    Log.info (fun m -> m "Not starting routines")

let start_server () =
  Log.info (fun m -> m "Starting server");
  let server =
    Lwt.catch
      (fun () ->
         Server.create
           ~mode:(`TCP (`Port !Dancelor_server_config.port))
           (Server.make ~callback ()))
      (fun exn ->
         log_exn ~msg:"Uncaught Lwt exception in the server" exn;
         Lwt.return ())
  in
  Log.info (fun m -> m "Server is up and running");
  try
    server
  with
    exn ->
    log_exn ~msg:"Uncaught exception in the server" exn;
    Lwt.return ()

let main =
  try
    try%lwt
      read_configuration ();
      initialise_logs ();
      initialise_database (); %lwt
        check_init_only ();
      start_routines ();
      start_server ()
    with
      exn ->
      log_exn ~msg:"Uncaught Lwt exception in main" exn;
      log_die ()
  with
    exn ->
    log_exn ~msg:"Uncaught exception in main" exn;
    log_die ()

let () = Lwt_main.run main
