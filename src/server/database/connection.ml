open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.connection": Logs.LOG)

module Sqlgg_lwt : Sqlgg_io.M with type 'a future = 'a Lwt.t = struct
  type 'a future = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
  let bracket x fin f =
    Lwt.bind x (fun x -> Lwt.finalize (fun () -> f x) (fun () -> fin x))
end

module Mariadb_lwt : Mariadb.Nonblocking.S with type 'a future = 'a Lwt.t = struct
  (* FIXME: I don't understand why this is needed? Maybe try updating `mariadb` in nixpkgs. *)
  type 'a future = 'a Lwt.t

  include Mariadb.Nonblocking.Make(struct
    module IO = Sqlgg_lwt

    let wait conn status =
      let module M = Mariadb.Nonblocking in
      (* REVIEW: This registers the [fd] in Lwt's event loop every time the
         function is called, so we might want something smarter. *)
      let fd = Lwt_unix.of_unix_file_descr @@ M.fd conn in
      let waits =
        let wait_read = if M.Status.read status then [Lwt_unix.wait_read fd] else [] in
        let wait_write = if M.Status.write status then [Lwt_unix.wait_write fd] else [] in
        let wait_timeout =
          if M.Status.timeout status then
            let ms = M.timeout_ms conn in
            (* REVIEW: [lwt_unit], really? Shouldn't we just raise a timeout immediately? *)
            [if ms <= 0 then lwt_unit else Lwt_unix.timeout (float_of_int ms /. 1000.)]
          else
              []
        in
        wait_read @ wait_write @ wait_timeout
      in
      (* REVIEW: This function doesn't handle the [except] case. Basically, we
         would want [Unix.select], but that's not really the spirit of Lwt. *)
      assert (waits <> []);
      Lwt.catch
        (fun () ->
          Lwt.map
            (fun _ ->
              M.Status.create
                ~read: (Lwt_unix.readable fd)
                ~write: (Lwt_unix.writable fd)
                ()
            )
            (Lwt.pick waits)
        )
        (function
          | Lwt_unix.Timeout -> lwt @@ M.Status.create ~timeout: true ()
          | exn -> Lwt.reraise exn
        )
  end)
end

module Sqlgg_mariadb_lwt = Sqlgg_mariadb.Default(Sqlgg_lwt)(Mariadb_lwt)

(* FIXME: the following API should lie, and not actually [open_], [close], and
   [with_]. It should maintain a pool of open connections to MariaDB and provide
   them when requested, keeping track of which ones are busy, etc. This would
   avoid having to pay for the handshake every time. *)

type t = Mariadb_lwt.t

exception Connection_failed of int * string

let actually_open () : t Lwt.t =
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
  let res =
    Mariadb_lwt.connect
      ?host
      ?port
      ?socket
      ~db
      ~user
      ?pass
      ~options: [
        (* handle CR_SERVER_GONE_ERROR transparently *)
        Reconnect true;
        (* don't hang forever on initial connect *)
        Connect_timeout 10;
        (* abort if server stops sending mid-query *)
        Read_timeout 30;
        (* abort if server stops receiving *)
        Write_timeout 10;
        (* surface data integrity issues explicitly *)
        Report_data_truncation true;
      ]
      ()
  in
  match%lwt res with
  | Error (no, msg) -> Lwt.fail (Connection_failed (no, msg))
  | Ok db -> lwt db

let actually_close (db : t) : unit Lwt.t =
  Mariadb_lwt.close db

(* a pool of free connections *)
let (pool, push) = Lwt_stream.create ()

(* the total number of connections *)
let number = ref 0

(* the min number of connections that should be kept open and the max number
   that is allowed; higher means that we don't need to establish connections
   as much but we keep a lot of dangling connections *)
let min_number = 10

(* the max allowed number of connections *)
let max_number = 100

(* NOTE: The logic before the first Lwt yield point must remain synchronous so
   that the read of [!number] and the [incr]/[decr] are atomic under Lwt's
   cooperative scheduling. *)
let open_ () : t Lwt.t =
  Log.debug (fun m -> m "open; %d <= %d <= %d" min_number !number max_number);
  match Lwt_stream.get_available_1 pool with
  | Some conn ->
    (* there is an available connection, take it *)
    lwt conn
  | None when !number < max_number ->
    (* there is space for more connections, open one *)
    incr number;
    actually_open ()
  | None ->
    (* there is no space for more connections, wait *)
    Log.warn (fun m -> m "Stalled because max number (%d) of connections is reached" max_number);
    Option.get <$> Lwt_stream.get pool

(* NOTE: Same atomicity requirement as [open_] above. *)
let close conn =
  Log.debug (fun m -> m "close; %d <= %d <= %d" min_number !number max_number);
  if !number > min_number then
    (
      (* if we are above the minimum number, then we close it *)
      decr number;
      actually_close conn
    )
  else
    (* otherwise, we just push the connection back onto the free pool *)
    lwt @@ push (Some conn)

let with_ (f : t -> 'a Lwt.t) : 'a Lwt.t =
  let%lwt db = open_ () in
  Lwt.finalize (fun () -> f db) (fun () -> close db)
