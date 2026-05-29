open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.connection": Logs.LOG)

let () =
  Printexc.register_printer @@ function
    | Postgresql.Error e -> Some ("PostgreSQL: " ^ Postgresql.string_of_error e)
    | _ -> None

let unix_fd_of_int : int -> Unix.file_descr = Obj.magic

type t = Sqlgg_postgresql.pg_conn

let pool_size = 10

let open_ () : t Lwt.t =
  Log.debug (fun m -> m "Opening new database connection");
  let cfg = (Config.get ()).Config.database in
  let (* assert *) PostgreSQL = cfg.Config.driver in
  let (host, port) =
    match cfg.Config.endpoint with
    | Address (host, port) -> (host, Some (string_of_int port))
    | Socket socket ->
      let socket = if Filename.is_relative socket then Filename.concat (Sys.getcwd ()) socket else socket in
        (socket, None)
  in
  let conn =
    new Postgresql.connection
      ~host
      ?port
      ~dbname: cfg.Config.database
      ~user: cfg.Config.user
      ?password: cfg.Config.password
      ()
  in
  conn#set_nonblocking true;
  ignore (conn#exec "SET client_min_messages TO WARNING");
  conn#set_notice_processor (fun s ->
    Log.warn (fun m -> m "PostgreSQL: %s" (String.trim s))
  );
  let fd = Lwt_unix.of_unix_file_descr ~blocking: false (unix_fd_of_int conn#socket) in
  Lwt.return Sqlgg_postgresql.{conn; fd}

let pool : t Lwt_pool.t =
  Lwt_pool.create
    pool_size
    ~validate: (fun {Sqlgg_postgresql.conn; _} -> Lwt.return (conn#status = Postgresql.Ok))
    ~dispose: (fun {Sqlgg_postgresql.conn; _} -> conn#finish; Lwt.return_unit)
    (fun () -> open_ ())

let result_status_is_ok result =
  match result#status with
  | Postgresql.Empty_query | Command_ok | Tuples_ok | Copy_out | Copy_in | Copy_both | Single_tuple -> true
  | Bad_response | Nonfatal_error | Fatal_error -> false

let bypass_exec ?(debug_log = true) (db : t) (query : string) =
  let result = db.Sqlgg_postgresql.conn#exec query in
  if debug_log then
    Log.debug (fun m ->
      m
        "bypass_exec: %a@\n%a"
        (Format.pp_multiline_sensible (Postgresql.result_status result#status))
        result#error
        (Format.pp_multiline_sensible "while executing")
        query
    );
  result

let with_transaction (db : t) (f : unit -> 'a Lwt.t) =
  assert (result_status_is_ok @@ bypass_exec db "BEGIN");
  try%lwt
    let%lwt r = f () in
    assert (result_status_is_ok @@ bypass_exec db "COMMIT");
    lwt r
  with
    | exn -> assert (result_status_is_ok @@ bypass_exec db "ROLLBACK"); Lwt.reraise exn

let with_ ?(transaction = true) (f : t -> 'a Lwt.t) : 'a Lwt.t =
  Lwt_pool.use pool @@ fun db ->
  if transaction then with_transaction db (fun () -> f db)
  else f db

let bypass_exec (db : t) (query : string) =
  let result = bypass_exec ~debug_log: false db query in
  (* NOTE: Using [bypass_exec] is always at least a warning. *)
  match result#status with
  | Empty_query | Command_ok | Tuples_ok | Copy_out | Copy_in | Copy_both | Single_tuple ->
    Log.warn (fun m ->
      m
        "bypass_exec: %s@\n%a"
        (Postgresql.result_status result#status)
        (Format.pp_multiline_sensible "while executing")
        query
    )
  | Bad_response | Nonfatal_error | Fatal_error ->
    Log.err (fun m ->
      m
        "bypass_exec: %a@\n%a"
        (Format.pp_multiline_sensible (Postgresql.result_status result#status))
        result#error
        (Format.pp_multiline_sensible "while executing")
        query
    );
    failwith "bypass_exec"
