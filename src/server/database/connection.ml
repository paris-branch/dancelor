open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.connection": Logs.LOG)

let () =
  Printexc.register_printer @@ function
    | Postgresql.Error e -> Some ("PostgreSQL: " ^ Postgresql.string_of_error e)
    | _ -> None

type t = Postgresql.connection

let connection : t option ref = ref None

let open_ () : t =
  match !connection with
  | Some conn -> conn
  | None ->
    Log.debug (fun m -> m "Connecting to database");
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
    ignore (conn#exec "SET client_min_messages TO WARNING");
    conn#set_notice_processor (fun s ->
      Log.warn (fun m -> m "PostgreSQL: %s" (String.trim s))
    );
    connection := Some conn;
    conn

let with_ (f : t -> 'a Lwt.t) : 'a Lwt.t =
  f (open_ ())
