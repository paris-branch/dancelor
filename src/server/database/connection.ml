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

let with_ (f : t -> 'a Lwt.t) : 'a Lwt.t =
  Lwt_pool.use pool f
