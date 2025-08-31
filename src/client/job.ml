open Nes
open Common
open Html

let rec file_href slug jobId =
  let%lwt response = Madge_client.call_exn Endpoints.Api.(route @@ Job Status) jobId in
  match response.status with
  | Pending | Running -> Js_of_ocaml_lwt.Lwt_js.sleep 1.;%lwt file_href slug jobId
  | Failed -> lwt_error response
  | Succeeded -> lwt_ok @@ Endpoints.Api.(href @@ Job File) jobId slug

let file_href slug route =
  Madge_client.call_gen route @@ function
    | Error error -> raise (Madge_client.Error error)
    | Ok jobId -> file_href slug jobId

let show_stdout_and_stderr ?collapse_id (response : Endpoints.Job.Response.t) =
  let collapse_a =
    match collapse_id with
    | None -> [a_class ["mt-4"]]
    | Some id -> [a_class ["mt-4"; "collapse"]; a_id id]
  in
  div ~a: collapse_a [
    match response.stderr with
    | "" -> span ~a: [a_class ["fst-italic"]] [txt "The output is empty."]
    | stderr -> pre [small [txt stderr]]
  ]
