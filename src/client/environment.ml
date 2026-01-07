open Nes
open Common
open Html

type run_status = Running | Offline | Newer

let (run_status, set_run_status) = S.create Running

let start_ping_routine () =
  let boot_time = ref None in
  let rec ping () =
    let old_boot_time = !boot_time in
    let%lwt response = Madge_client.call ~retry: false Endpoints.Api.(route BootTime) in
    (
      match response with
      | Ok new_boot_time ->
        boot_time := Some new_boot_time;
        set_run_status (
          match old_boot_time with
          | Some old_boot_time when not (Datetime.equal old_boot_time new_boot_time) -> Newer
          | _ -> Running
        )
      | Error Server_unreachable _ -> set_run_status Offline
      | Error _ -> assert false
    );
    Js_of_ocaml_lwt.Lwt_js.sleep 3.;%lwt
    ping ()
  in
  Lwt.async ping

let user = Madge_client.call_exn Endpoints.Api.(route @@ User Status)

let user_now () = match Lwt.state user with Return user -> user | _ -> None
