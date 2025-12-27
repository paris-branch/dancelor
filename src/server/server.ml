open Cohttp_lwt_unix

open Nes
open Common

module Log = (val Logger.create "": Logs.LOG)

let log_exn ~msg exn =
  Log.err @@ fun m ->
  let repr =
    match exn with
    | Database.Error.Exn error ->
      "Common.Error." ^ Database.Error.show error
    | exn -> Printexc.to_string exn
  in
  m "%a" (Format.pp_multiline_sensible msg) (repr ^ "\n" ^ (Printexc.get_backtrace ()))

let log_exit = Logger.log_exit (module Log)
let log_die () = Logger.log_die (module Log)

include Madge_server.Make_apply_controller(struct
  include Endpoints.Api
  type env = Environment.t
  let dispatch = Controller.dispatch
end)

(** Wraps a function into a double catchall: regular exceptions and Lwt
    exceptions. Exceptions are logged as uncaught, and then the `die` function
    is called. *)
let catchall ~place ~die fun_ =
  try
    try%lwt
      fun_ ()
    with
      | exn ->
        log_exn ~msg: ("Uncaught Lwt exception in " ^ place) exn;
        die ()
  with
    | exn ->
      log_exn ~msg: ("Uncaught exception in " ^ place) exn;
      die ()

(** Callback handling one client request. It is in charge of trying to find what
    will answer to the request: a static file, or a Madge API point, or the
    standard main JS file. *)
let callback _ request body =
  Metrics.increment_http_requests_total ();
  catchall
    ~place: "the callback"
    ~die: Madge_server.respond_internal_server_error
    @@ fun () ->
    let meth = Madge.Request.cohttp_code_meth_to_meth @@ Request.meth request in
    let uri = Request.uri request in
    let path = Uri.path uri in
    Log.info (fun m -> m "%s %s" (Madge.Request.meth_to_string meth) path);
    Environment.with_ request @@ fun env ->
    if String.starts_with ~needle: "/api/" path then
      (
        Log.debug (fun m -> m "Looking for an API controller for %s." path);
        let%lwt body = Cohttp_lwt.Body.to_string body in
        apply_controller env (Madge.Request.make ~meth ~uri ~body)
      )
    else
      Static.serve path

  let () =
    Lwt.async_exception_hook :=
      (function
        | Unix.Unix_error (Unix.EPIPE, _, _) ->
          Log.warn (fun m -> m "Connection closed by the client")
        | exn ->
          log_exn ~msg: "Uncaught asynchronous exception" exn
      )

  let read_configuration () =
    Log.info (fun m -> m "Reading configuration");
    Config.parse_cmd_line ()

  let initialise_logs () =
    Logger.initialise !Config.loglevel

  let write_pid () =
    let pid = Unix.getpid () in
    if !Config.pid_file <> "" then
      Lwt_io.(
        with_file ~mode: output !Config.pid_file @@ fun ochan ->
        fprintlf ochan "%d" pid
      )
    else
      lwt_unit

  let initialise_database () =
    Log.info (fun m -> m "Initialising database");
    Database.Tables.initialise ()

  let check_init_only () =
    if !Config.init_only then
      (
        Log.info (fun m -> m "Init only mode. Stopping now.");
        log_exit 0
      )

  let start_routines () =
    Log.info (fun m -> m "Starting routines");
    Routine.initialise ()

  let run_server () =
    Log.info (fun m -> m "Starting server");
    catchall
      ~place: "the server"
      ~die: log_die
      @@ fun () ->
      let server =
        Server.create
          ~mode: (`TCP (`Port !Config.port))
          (Server.make ~callback ())
      in
      Log.info (fun m -> m "Server is up and running");
      server

  let () = Random.self_init ()

  let main =
    catchall
      ~place: "main"
      ~die: log_die
      @@ fun () ->
      read_configuration ();%lwt
      initialise_logs ();
      write_pid ();%lwt
      initialise_database ();%lwt
      check_init_only ();
      start_routines ();
      run_server ()

  let () = Lwt_main.run main
