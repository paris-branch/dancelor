open Nes
open Dancelor_common
open Dancelor_server_controller
open Cohttp_lwt_unix
module Log = (val Dancelor_server_logs.create "main" : Logs.LOG)

type query = (string * string list) list
[@@deriving show]

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
         QueryParameters.t ->
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

(** Consider the query and the body to build a consolidated query. *)
let consolidate_query_parameters (uri : Uri.t) (body : Cohttp_lwt.Body.t) : QueryParameters.t Lwt.t =
  let%lwt high = QueryParameters.from_body body in
  let low = QueryParameters.from_uri uri in
  Lwt.return (QueryParameters.append ~high ~low)

(** Wraps a function into a double catchall: regular exceptions and Lwt
   exceptions. Exceptions are logged as uncaught, and then the `die` function is
   called. *)
let catchall ~place ~die fun_ =
  try
    try%lwt
      fun_ ()
    with
      exn ->
      log_exn ~msg:("Uncaught Lwt exception in "^place) exn;
      die ()
  with
    exn ->
    log_exn ~msg:("Uncaught exception in "^place) exn;
    die ()

(** Callback handling one client request. It is in charge of trying to find what
   will answer to the request: a static file, or a Madge API point, or the
   standard main JS file. *)
let callback _ request body =
  catchall
    ~place:"the callback"
    ~die:(Server.respond_error ~status:`Internal_server_error ~body:"{}")
  @@ fun () ->
  let uri = Request.uri request in
  let meth = Request.meth request in
  let path = Uri.path uri in
  Log.info (fun m -> m "Request for %s" path);
  let%lwt query_parameters = consolidate_query_parameters uri body in
  let full_path = Filename.concat_l [ !Dancelor_server_config.share; "static"; path ] in
  Log.debug (fun m -> m "Looking for %s" full_path);
  if Sys.file_exists full_path && not (Sys.is_directory full_path) then
    (
      Log.debug (fun m -> m "Serving static file.");
      Server.respond_file ~fname:full_path ()
    )
  else
    (
      Log.debug (fun m -> m "Asking Madge for %s." path);
      match%lwt Madge_server.handle meth path (QueryParameters.to_list query_parameters) with
      | Some response -> Lwt.return response
      | None ->
        if String.length path >= 5 && String.sub path 0 5 = "/"^Constant.api_prefix^"/" then
          (
            let path = String.sub path 4 (String.length path - 4) in
            Log.debug (fun m -> m "Looking for an API controller for %s." path);
            apply_controller path query_parameters
          )
        else
          (
            Log.debug (fun m -> m "Serving main file.");
            Server.respond_file ~fname:Filename.(concat (concat !Dancelor_server_config.share "static") "index.html") ()
          )
    )

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

let run_server () =
  Log.info (fun m -> m "Starting server");
  catchall
    ~place:"the server"
    ~die:log_die
  @@ fun () ->
  let server =
    Server.create
      ~mode:(`TCP (`Port !Dancelor_server_config.port))
      (Server.make ~callback ())
  in
  Log.info (fun m -> m "Server is up and running");
  server

let main =
  catchall
    ~place:"main"
    ~die:log_die
  @@ fun () ->
  read_configuration ();
  initialise_logs ();
  initialise_database (); %lwt
  check_init_only ();
  start_routines ();
  run_server ()

let () = Lwt_main.run main
