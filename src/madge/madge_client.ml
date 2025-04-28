open Nes
include Madge

exception HttpError of {
    request: Madge.Request.t;
    status: Cohttp.Code.status_code;
    message: string option;
  }

let httpError request status message = raise @@ HttpError {request; status; message}

type error_response = {message: string option [@default None]} [@@deriving yojson]

let call
  : type a r. ?on_error: (Madge.Request.t -> Cohttp.Code.status_code -> string option -> r Lwt.t) ->
  (a, r Lwt.t, r) Route.t ->
  a
= fun ?(on_error = httpError) route ->
  with_request route @@ fun (module R) ({meth; uri; body} as request) ->
  let meth = Request.meth_to_cohttp_code_meth meth in
  let body = Cohttp_lwt.Body.of_string body in
  let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth uri ~body in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let body =
    try
      Yojson.Safe.from_string body
    with
      | Yojson.Json_error _fixme -> failwith "Madge_client.call: body is not JSON"
  in
  let status = Cohttp.Response.status response in
  if Cohttp.(Code.(is_success (code_of_status status))) then
    (
      match R.of_yojson body with
      | Error msg -> failwith @@ "Madge_client.call: body cannot be unserialised: " ^ msg
      | Ok body -> Lwt.return body
    )
  else
    (
      match error_response_of_yojson body with
      | Error msg -> failwith @@ "Madge_client.call: error body cannot be unserialised: " ^ msg
      | Ok {message} -> on_error request status message
    )
