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
  | `GET -> Ok GET
  | `POST -> Ok POST
  | `HEAD -> Ok HEAD
  | `DELETE -> Ok DELETE
  | `PATCH -> Ok PATCH
  | `PUT -> Ok PUT
  | `OPTIONS -> Ok OPTIONS
  | `TRACE -> Ok TRACE
  | `CONNECT -> Ok CONNECT
  | `Other x -> Error x

let is_safe = function
  | GET | HEAD | OPTIONS | TRACE -> true
  | PUT | DELETE | PATCH | POST | CONNECT -> false

type t = {
  meth: meth;
  uri: Uri.t;
  body: string;
}
[@@deriving make, fields, yojson]
