open Nes
include Madge

type http_error = {
  request: Request.t;
  status: Cohttp.Code.status_code;
  message: string;
}

exception HttpError of http_error
exception ServerUnreachable of Request.t
exception BodyUnserialisationError of string

type error_response = {message: string} [@@deriving yojson]

let max_attempts = 10 (* up to ~2 minutes *)

let rec call_retry ?(attempt = 1) (request : Request.t) =
  let meth = Request.meth_to_cohttp_code_meth request.meth in
  let body = Cohttp_lwt.Body.of_string request.body in
  let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth request.uri ~body in
  let status = Cohttp.Response.status response in
  if Cohttp.Code.code_of_status status = 0 then
    (
      if attempt >= max_attempts then
        raise (ServerUnreachable request)
      else
        let delay = (2. ** (float_of_int attempt) +. Random.float 2.) /. 8. in
        Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt
        call_retry ~attempt: (attempt + 1) request
    )
  else
    (
      Lwt.return (status, body)
    )

let call_gen
  : type a r z. (a, z Lwt.t, r) Route.t ->
  ((r, http_error) result -> z) ->
  a
= fun route cont ->
  with_request route @@ fun (module R) request ->
  let%lwt (status, body) = call_retry request in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let body =
    try
      Yojson.Safe.from_string body
    with
      | Yojson.Json_error msg -> raise (BodyUnserialisationError ("not JSON: " ^ msg))
  in
  let result =
    if Cohttp.(Code.(is_success (code_of_status status))) then
      (
        match R.of_yojson body with
        | Error msg -> raise (BodyUnserialisationError msg)
        | Ok body -> Ok body
      )
    else
      (
        match error_response_of_yojson body with
        | Error msg -> raise (BodyUnserialisationError ("expected an error, but " ^ msg))
        | Ok {message} -> Error {request; status; message}
      )
  in
  Lwt.return @@ cont result

let call
  : type a r. (a, (r, http_error) result Lwt.t, r) Route.t -> a
= fun route -> call_gen route Fun.id

let call_exn
  : type a r. (a, r Lwt.t, r) Route.t -> a
= fun route -> call_gen route @@ Result.fold ~ok: Fun.id ~error: (fun err -> raise (HttpError err))
