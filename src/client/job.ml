open Nes
open Common

(* FIXME: [file_href] is a bit of a hack. At least, things should work, but we
   should do better with handling the errors and stuff. *)

let rec file_href slug jobId =
  let%lwt response = Madge_client.call_exn Endpoints.Api.(route @@ Job Status) jobId in
  match response.status with
  | Running -> Js_of_ocaml_lwt.Lwt_js.sleep 1.;%lwt file_href slug jobId
  | Failed -> assert false
  | Succeeded -> lwt @@ Endpoints.Api.(href @@ Job File) jobId slug

let file_href slug route =
  Madge_client.call_gen route @@ function
    | Error _ -> assert false
    | Ok jobId -> file_href slug jobId
