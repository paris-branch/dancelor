open Nes

type meth =
  GET | POST | HEAD | DELETE | PATCH | PUT | OPTIONS | TRACE | CONNECT
[@@deriving yojson]

let meth_to_string = function
  | GET -> "GET"
  | POST -> "POST"
  | HEAD -> "HEAD"
  | DELETE -> "DELETE"
  | PATCH -> "PATCH"
  | PUT -> "PUT"
  | OPTIONS -> "OPTIONS"
  | TRACE -> "TRACE"
  | CONNECT -> "CONNECT"

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

let cohttp_code_meth_to_meth = function
  | `GET -> GET
  | `POST -> POST
  | `HEAD -> HEAD
  | `DELETE -> DELETE
  | `PATCH -> PATCH
  | `PUT -> PUT
  | `OPTIONS -> OPTIONS
  | `TRACE -> TRACE
  | `CONNECT -> CONNECT
  | `Other _ -> assert false (* FIXME *)

let is_safe = function
  | GET | HEAD | OPTIONS | TRACE -> true
  | PUT | DELETE | PATCH | POST | CONNECT -> false

type t = {
  meth: meth;
  uri: Uri.t;
  body: string;
}
[@@deriving make, fields, yojson]
