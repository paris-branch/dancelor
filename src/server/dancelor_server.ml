open Nes
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
  Log.err (fun m -> m "%s@\n%s@\n%a" msg (Printexc.to_string exn) pp_string_multiline (Printexc.get_backtrace ()))

let bad_gateway ?(msg="") _ =
  Server.respond_string
    ~status:`Bad_gateway
    ~body:("<html><head><title>502 Bad Gateway</title></head><body bgcolor=\"white\"><center><h1>502 Bad Gateway</h1></center><hr><center>" ^ msg ^ "</center></body></html>")
    ()

let respond_json ?(status=`OK) json =
  Server.respond_string ~status ~body:(Json.to_string json) ()

let apply_controller (controller : 'a Controller.t) (serializer : 'a -> NesJson.t) query =
  let open Dancelor_common.Error in
  try%lwt
    let%lwt val_ = controller query in
    respond_json (serializer val_)
  with
  | Exn error -> (* Expected failure. *)
    respond_json ~status:(status error) (to_yojson error)
  | exn -> (* Unexpected failure. *)
    log_exn ~msg:"Uncaught exception in controller" exn;
    respond_json ~status:(status Unexpected) (to_yojson Unexpected)

let list_serializer = Dancelor_common.Serializer.list

let apply_controller = let open Dancelor_common.Router in function
    | Credit credit -> apply_controller (Credit.get credit) Dancelor_server_model.Credit.to_yojson

    | Dance dance -> apply_controller (Dance.get dance) Dancelor_server_model.Dance.to_yojson

    | Pascaline -> bad_gateway ~msg:"Pour Pascaline Latour"

    | Person person -> apply_controller (Person.get person) Dancelor_server_model.Person.to_yojson

    | ProgramAll -> apply_controller Program.get_all (list_serializer Dancelor_server_model.Program.to_yojson)
    | ProgramPdf program -> Program.Pdf.get program
    | Program program -> apply_controller (Program.get program) Dancelor_server_model.Program.to_yojson

    | SetAll -> apply_controller Set.get_all (list_serializer Dancelor_server_model.Set.to_yojson) (* FIXME: list of *)
    | SetSave -> apply_controller Set.save Dancelor_server_model.Set.to_yojson
    | SetLy set -> Set.Ly.get set
    | SetPdf set -> Set.Pdf.get set
    | Set set -> apply_controller (Set.get set) Dancelor_server_model.Set.to_yojson
    | SetDelete set -> apply_controller (Set.delete set) (fun () -> `Assoc []) (* FIXME: unit json *)

    | TuneGroup tune_group -> apply_controller (TuneGroup.get tune_group) Dancelor_server_model.TuneGroup.to_yojson

    | TuneAll -> apply_controller Tune.get_all (list_serializer Dancelor_server_model.(Score.to_yojson Tune.to_yojson)) (* FIXME: list of *)
    | TuneLy tune -> Tune.get_ly tune
    | TunePng tune -> Tune.Png.get tune
    | Tune tune -> apply_controller (Tune.get tune) Dancelor_server_model.Tune.to_yojson

    | Victor -> exit 0

    (* Routes that are not API points. *)
    | Index | SetCompose -> (fun _ -> Server.respond_not_found ())


let callback _ request _body =
  (* We have a double try ... with to catch all non-Lwt and Lwt
     exceptions. *)
  try
    try%lwt
      let uri = Request.uri request in
      let meth = Request.meth request in
      let path = Uri.path uri in
      Log.info (fun m -> m "Request for %s" path);

      let full_path = Filename.(concat (concat !Dancelor_server_config.share "static") path) in
      Log.debug (fun m -> m "Looking for %s" full_path);
      if Sys.file_exists full_path && not (Sys.is_directory full_path) then
        (
          Log.debug (fun m -> m "Serving static file.");
          Server.respond_file ~fname:full_path ()
        )
      else if String.length path >= 5 && String.sub path 0 5 = "/"^Constant.api_prefix^"/" then
        (
          let path = String.sub path 4 (String.length path - 4) in
          Log.debug (fun m -> m "Looking for an API controller for %s." path);
          match Dancelor_common.Router.path_to_controller ~meth ~path with
          | None ->
            Log.debug (fun m -> m "Could not find a controller.");
            Server.respond_not_found ~uri ()
          | Some controller ->
            Log.debug (fun m -> m "Controller found");
            let query = cleanup_query (Uri.query uri) in
            Log.debug (fun m -> m "Query: %a" pp_query query);
            apply_controller controller query
        )
      else
        (
          Log.debug (fun m -> m "Serving main file.");
          Server.respond_file ~fname:Filename.(concat (concat !Dancelor_server_config.share "static") "index.html") ()
        )
    with
    | Dancelor_common.Error.Exn err ->
      Dancelor_common.Error.(respond_json ~status:(status err) (to_yojson err))
    | exn ->
      log_exn ~msg:"Uncaught exception in the callback" exn;
      Server.respond_error ~status:`Internal_server_error ~body:"{}" ()
  with
  | Dancelor_common.Error.Exn err ->
    Dancelor_common.Error.(respond_json ~status:(status err) (to_yojson err))
  | exn ->
    log_exn ~msg:"Uncaught Lwt exception in the callback" exn;
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

let initialise_database () =
  Log.info (fun m -> m "Initialising database");
  if (not !Dancelor_server_config.init_only) && !Dancelor_server_config.sync_storage then
    Dancelor_server_database.Storage.sync_changes ();
  let%lwt () = Dancelor_server_database.initialise () in
  Dancelor_server_database.report_without_accesses ();
  Lwt.return ()

let check_init_only () =
  if !Dancelor_server_config.init_only then
    (
      Log.info (fun m -> m "Init only mode. Stopping now.");
      exit 0
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
  read_configuration ();
  let%lwt () = initialise_database () in
  check_init_only ();
  start_routines ();
  start_server ()

let () = Lwt_main.run main
