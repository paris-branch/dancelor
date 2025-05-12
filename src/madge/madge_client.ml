open Ppx_yojson_conv_lib.Yojson_conv
open Nes
include Madge

type http_error = {
  request: Madge.Request.t;
  status: Cohttp.Code.status_code;
  message: string;
}

exception HttpError of http_error

type error_response = {message: string} [@@deriving yojson]

let call_gen
  : type a r z. (a, z Lwt.t, r) Route.t ->
  ((r, http_error) result -> z) ->
  a
= fun route cont ->
  with_request route @@ fun (module R) ({meth; uri; body} as request) ->
  let meth = Request.meth_to_cohttp_code_meth meth in
  let body = Cohttp_lwt.Body.of_string body in
  let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth uri ~body in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let body =
    try
      Yojson.Safe.from_string body
    with
      | Yojson.Json_error msg -> failwith @@ "Madge_client.call: body is not JSON: " ^ msg
  in
  let status = Cohttp.Response.status response in
  let result =
    if Cohttp.(Code.(is_success (code_of_status status))) then
      (
        try
          Ok (R.t_of_yojson body)
        with
          | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _ ->
            failwith "Madge_client.call: body cannot be unserialised"
      )
    else
      (
        try
          let {message} = error_response_of_yojson body in Error {request; status; message}
        with
          | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _ ->
            failwith "Madge_client.call: error body cannot be unserialised"
      )
  in
  Lwt.return @@ cont result

let call
  : type a r. (a, (r, http_error) result Lwt.t, r) Route.t -> a
= fun route -> call_gen route Fun.id

let call_exn
  : type a r. (a, r Lwt.t, r) Route.t -> a
= fun route -> call_gen route @@ Result.fold ~ok: Fun.id ~error: (fun err -> raise (HttpError err))
