open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

exception IO_error of string

let meth_to_string = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `PUT -> "PUT"
  | `OPTIONS -> "OPTIONS"
  | `TRACE -> "TRACE"
  | `CONNECT -> "CONNECT"
  | `Other s -> s

let request ?query ~reader ~route () = 
  let meth, path = Router.path_of_controller route in
  let full_path = Printf.sprintf "/%s%s" Constant.api_prefix path in
  let uri = Uri.make ~path:full_path ?query () in
  let%lwt (resp, body) = Cohttp_lwt_unix.Client.call meth uri in
  let%lwt contents = Cohttp_lwt.Body.to_string body in
  let yojson = NesJson.from_string contents in
  let code = Cohttp.Response.status resp in
  if Cohttp.Code.is_success (Cohttp.Code.code_of_status code) then begin
    match reader yojson with
    | Ok v -> Lwt.return v
    | Error s -> Lwt.fail (IO_error s)
  end else begin
    match Error.of_yojson yojson with
    | Ok v -> Lwt.fail (Error.Exn v)
    | Error s -> Lwt.fail (IO_error s)
  end
