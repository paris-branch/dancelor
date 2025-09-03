open Nes
include Madge

type error =
  | Http of {request: Request.t; status: Cohttp.Code.status_code; message: string}
  | ServerUnreachable of {request: Request.t; status: Cohttp.Code.status_code}
  | BodyUnserialisation of {body: string; message: string}

exception Error of error

type error_response = {message: string} [@@deriving yojson]

let max_attempts = 10 (* up to ~2 minutes *)

let call_retry ?(retry = true) (request : Request.t) =
  let meth = Request.meth_to_cohttp_code_meth request.meth in
  let body = Cohttp_lwt.Body.of_string request.body in
  let rec call_retry attempt =
    let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth request.uri ~body in
    let status = Cohttp.Response.status response in
    if List.mem (Cohttp.Code.code_of_status status) [0; 502; 503; 504] then
      (
        if attempt >= max_attempts then
          Lwt.return_error @@ ServerUnreachable {request; status}
        else
          let delay = (2. ** (float_of_int attempt) +. Random.float 2.) /. 8. in
          Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt
          call_retry (attempt + 1)
      )
    else
      Lwt.return_ok (status, body)
  in
  call_retry (if retry then 1 else max_int)

let call_gen
  : type a r z. ?retry: bool ->
  (a, z, r) Route.t ->
  ((r, error) result Lwt.t -> z) ->
  a
= fun ?retry route cont ->
  with_request route @@ fun (module R) request ->
  cont @@
    let%rlwt (status, body) = call_retry ?retry request in
    let%lwt body = Cohttp_lwt.Body.to_string body in
    let%rlwt json_body =
      try
        Lwt.return_ok @@ Yojson.Safe.from_string body
      with
        | Yojson.Json_error message ->
          Lwt.return_error @@ BodyUnserialisation {body; message = "not JSON: " ^ message}
    in
    if Cohttp.(Code.(is_success (code_of_status status))) then
      (
        match R.of_yojson json_body with
        | Error message -> Lwt.return_error @@ BodyUnserialisation {body; message}
        | Ok body -> Lwt.return_ok body
      )
    else
      (
        match error_response_of_yojson json_body with
        | Error message -> Lwt.return_error @@ BodyUnserialisation {body; message = "expected an error, but " ^ message}
        | Ok {message} -> Lwt.return_error @@ Http {request; status; message}
      )

let call
  : type a r. ?retry: bool -> (a, (r, error) result Lwt.t, r) Route.t -> a
= fun ?retry route -> call_gen ?retry route id

let call_exn
  : type a r. ?retry: bool -> (a, r Lwt.t, r) Route.t -> a
= fun ?retry route -> call_gen ?retry route @@ Lwt.map @@ Result.fold ~ok: Fun.id ~error: (fun e -> raise (Error e))
