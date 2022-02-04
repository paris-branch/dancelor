open Nes
open Dancelor_common

exception IO_error of string

include Dancelor_client_elements.JsHelpers

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

let build_path ?(api=false) ?query ~route () =
  let _, path = Router.path_of_controller route in
  let full_path =
    if api then spf "/%s%s" Constant.api_prefix path
    else path
  in
  let uri = Uri.make ~path:full_path ?query () in
  Uri.to_string uri
