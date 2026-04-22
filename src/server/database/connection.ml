open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.connection": Logs.LOG)

exception Connection_failed of int * string

let open_ () =
  Log.debug (fun m -> m "Connecting to database");
  let MariaDB cfg = (Config.get ()).Config.database in
  let (host, port, socket) =
    match cfg.endpoint with
    | Address (host, port) -> (Some host, Some port, None)
    | Socket socket -> (None, None, Some socket)
  in
  let db = cfg.database in
  let user = cfg.user in
  let pass = cfg.password in
  match%lwt Sqlgg_mariadb_lwt.Mariadb.connect ?host ?port ?socket ~db ~user ?pass () with
  | Error (no, msg) -> Lwt.fail (Connection_failed (no, msg))
  | Ok db -> lwt db

let close db =
  Sqlgg_mariadb_lwt.Mariadb.close db

let with_ f =
  let%lwt db = open_ () in
  Lwt.finalize (fun () -> f db) (fun () -> close db)
