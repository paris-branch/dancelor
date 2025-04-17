open Nes
include Madge

let meth_to_cohttp_code_meth = function
  | GET -> `GET
  | POST -> `POST
  | HEAD -> `HEAD
  | DELETE -> `DELETE
  | PATCH -> `PATCH
  | PUT -> `PUT
  | OPTIONS -> `OPTIONS
  | TRACE -> `TRACE
  | CONNECT -> `CONNECT

exception HttpError of {
    request: Madge.request;
    status: Cohttp.Code.status_code;
    message: string option;
  }

type error_response = {message: string option [@default None]} [@@deriving yojson]

let call : type a r. (a, r Lwt.t, r) route -> a = fun route ->
  process route @@ fun (module R) ({meth; uri; body} as request) ->
  let meth = meth_to_cohttp_code_meth meth in
  let body = Cohttp_lwt.Body.of_string body in
  let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth uri ~body in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let body =
    try
      Yojson.Safe.from_string body
    with
      | Yojson.Json_error _fixme -> failwith "Madge_cohttp_lwt_client.call: body is not JSON"
  in
  let status = Cohttp.Response.status response in
  if Cohttp.(Code.(is_success (code_of_status status))) then
    (
      match R.of_yojson body with
      | Error msg -> failwith @@ "Madge_cohttp_lwt_client.call: body cannot be unserialised: " ^ msg
      | Ok body -> Lwt.return body
    )
  else
    (
      match error_response_of_yojson body with
      | Error msg -> failwith @@ "Madge_cohttp_lwt_client.call: error body cannot be unserialised: " ^ msg
      | Ok {message} -> raise @@ HttpError {request; status; message}
    )
