open Nes
open Dancelor_common
open Model_new
open Html

type server_status = Reachable | Unreachable

let (server_status, set_server_status) = S.create Reachable

let () = Madge_client.on_server_reachable := (fun () -> set_server_status Reachable)
let () = Madge_client.on_server_unreachable := (fun () -> set_server_status Unreachable)

let user = Madge_client.call_exn Endpoints.Api.(route @@ User Status)
let user_new = Madge_client.call_exn Endpoints.Api.(route @@ User Status_new)

let is_connected = Lwt.map Option.is_some user

let user_now () = match Lwt.state user with Return user -> user | _ -> None

let person_row =
  match%lwt user with
  | None -> lwt_none
  | Some user -> Madge_client.call_exn Endpoints.Api.(route @@ Person For_user) (Entry.id user)

let person_id =
  let%lwt person = person_row in
  lwt @@ Option.map (fun p -> p.Person_row.id) person
