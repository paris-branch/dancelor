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

let bad_gateway _ =
  Server.respond_error
    ~status:`Bad_gateway
    ~body:"<html><head><title>502 Bad Gateway</title></head><body bgcolor=\"white\"><center><h1>502 Bad Gateway</h1></center><hr><center>nginx/1.10.3</center></body></html>"
    ()

let apply_json_controller json_controller query =
  Lwt.bind
    (json_controller query)
    (fun json -> Server.respond_string ~status:`OK ~body:(Json.to_string json) ())

let apply_controller = let open Router in function
  | Index -> assert false
  | Credit credit -> apply_json_controller (Credit.get credit)
  | Pascaline -> bad_gateway
  | Person person -> apply_json_controller (Person.get person)
  | ProgramAll -> apply_json_controller Program.get_all
  | ProgramPdf program -> Program.Pdf.get program
  | Program program -> apply_json_controller (Program.get program)
  | SetAll -> apply_json_controller Set.get_all
  | SetCompose -> assert false
  | SetSave -> apply_json_controller Set.save
  | SetLy set -> Set.Ly.get set
  | SetPdf set -> Set.Pdf.get set
  | Set set -> apply_json_controller (Set.get set)
  | TuneGroup tune_group -> apply_json_controller (TuneGroup.get tune_group)
  | TuneAll -> apply_json_controller Tune.get_all
  | TuneLy tune -> Tune.get_ly tune
  | TunePng tune -> Tune.Png.get tune
  | Tune tune -> apply_json_controller (Tune.get tune)
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
          Log.debug (fun m -> m "Looking for a controller.");
          match Router.path_to_controller ~meth ~path with
          | None -> Server.respond_not_found ~uri ()
          | Some controller -> apply_controller controller (Uri.query uri)
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

  Log.info (fun m -> m "Starting routines");
  Routine.initialise ();

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
