open Dancelor_common
open Dancelor_controller
open Cohttp_lwt_unix
module Log = (val Log.create "dancelor.server" : Logs.LOG)

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
  Log.err (fun m -> m "%s@\n%s@\n%a" msg (Printexc.to_string exn) pp_string_multiline (Printexc.get_backtrace ()))

let bad_gateway ?(msg="") _ =
  Server.respond_string
    ~status:`Bad_gateway
    ~body:("<html><head><title>502 Bad Gateway</title></head><body bgcolor=\"white\"><center><h1>502 Bad Gateway</h1></center><hr><center>" ^ msg ^ "</center></body></html>")
    ()

let (>>=) = Lwt.bind

let apply_json_controller json_controller query =
  json_controller query >>= fun json ->
  Server.respond_string ~status:`OK ~body:(Json.to_string json) ()

let respond_view ?(status=`OK) view json =
  Server.respond_string ~status ~body:(View.render view json) ()

(* FIXME: once the client is fully converted to OCaml, everything here
   will be using the API. We will be able to remove all the notions of
   ~api and ~view in the next functions. *)

let apply_html_controller ~api ~view json_controller query =
  json_controller query >>= fun json ->
  let json = LinksAdder.json_add_links json in
  if api then
    Server.respond_string ~status:`OK ~body:(Json.to_string json) ()
  else
    respond_view view (Json.to_ezjsonm json)

let apply_controller ~api = let open Dancelor_router in function
  | Index -> (fun _ -> respond_view "/index" (`O []))
  | Credit credit -> apply_html_controller ~api ~view:"/credit" (Credit.get credit)
  | Pascaline -> bad_gateway ~msg:"Pour Pascaline Latour"
  | Person person -> apply_html_controller ~api ~view:"/person" (Person.get person)
  | ProgramAll -> apply_html_controller ~api ~view:"/program/all" Program.get_all
  | ProgramPdf program -> Program.Pdf.get program
  | Program program -> apply_html_controller ~api ~view:"/program" (Program.get program)
  | SetAll -> apply_html_controller ~api ~view:"/set/all" Set.get_all
  | SetCompose -> (fun _ -> respond_view "/set/compose" (`O []))
  | SetSave -> if api then apply_json_controller Set.save else (fun _ -> Server.respond_not_found ())
  | SetLy set -> Set.Ly.get set
  | SetPdf set -> Set.Pdf.get set
  | Set set -> apply_html_controller ~api ~view:"/set" (Set.get set)
  | TuneGroup tune_group -> apply_html_controller ~api ~view:"/tune-group" (TuneGroup.get tune_group)
  | TuneAll -> apply_html_controller ~api ~view:"/tune/all" Tune.get_all
  | TuneLy tune -> Tune.get_ly tune
  | TunePng tune -> Tune.Png.get tune
  | Tune tune -> apply_html_controller ~api ~view:"/tune" (Tune.get tune)
  | Victor -> exit 0

let callback _ request _body =
  (* We have a double try ... with to catch all non-Lwt and Lwt
     exceptions. *)
  try
    try%lwt
      let uri = Request.uri request in
      let meth = Request.meth request in
      let path = Uri.path uri in
      Log.info (fun m -> m "Request for %s" path);
      let full_path = Filename.(concat (concat !Config.share "static") path) in
      Log.debug (fun m -> m "Looking for %s" full_path);
      if Sys.file_exists full_path && not (Sys.is_directory full_path) then
        (
          Log.debug (fun m -> m "Serving static file.");
          Server.respond_file ~fname:full_path ()
        )
      else
        (
          let (api, path) =
            try
              if String.sub path 0 4 = "/api" then
                (true, String.sub path 4 (String.length path - 4)) (*FIXME: 4 or 5?*)
              else
                (false, path)
            with
              Invalid_argument _ -> (false, path)
          in
          Log.debug (fun m -> m "Looking for a controller.");
          match Dancelor_router.path_to_controller ~meth ~path with
          | None -> Server.respond_not_found ~uri ()
          | Some controller -> apply_controller ~api controller (Uri.query uri)
        )
    with
      exn ->
      log_exn ~msg:"Uncaught exception in the callback" exn;
      Server.respond_error ~status:`Internal_server_error ~body:"internal server error" ()
  with
    exn ->
    log_exn ~msg:"Uncaught Lwt exception in the callback" exn;
    Server.respond_error ~status:`Internal_server_error ~body:"internal server error" ()

let () =
  Lwt.async_exception_hook :=
    (function
     | Unix.Unix_error(Unix.EPIPE, _, _) ->
        Log.warn (fun m -> m "Connection closed by the client")
     | exn ->
        log_exn ~msg:"Uncaught asynchronous exception" exn)

let () =
  Log.info (fun m -> m "Reading configuration");
  Config.load_from_file Sys.argv.(1);

  Log.info (fun m -> m "Initialising database");
  Dancelor_model.Database.initialise ();

  if !Config.routines then
    (
      Log.info (fun m -> m "Starting routines");
      Routine.initialise ()
    )
  else
    Log.info (fun m -> m "Not starting routines");

  Log.info (fun m -> m "Starting server");
  let server =
    Lwt.catch
      (fun () ->
        Server.create
          ~mode:(`TCP (`Port !Config.port))
          (Server.make ~callback ()))
      (fun exn ->
        log_exn ~msg:"Uncaught Lwt exception in the server" exn;
        Lwt.return ())
  in
  Log.info (fun m -> m "Server is up and running");
  try
    Lwt_main.run server
  with
    exn ->
    log_exn ~msg:"Uncaught exception in the server" exn
