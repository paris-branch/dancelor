type meth =
  GET | POST | HEAD | DELETE | PATCH | PUT | OPTIONS | TRACE | CONNECT
[@@deriving biniou, yojson]

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

module Uri = struct
  include Uri
  let to_biniou u = `String (Uri.to_string u)
  let of_biniou_exn = function
    | `String s -> Uri.of_string s (* FIXME: Uri.of_string fails sometimes I suppose? *)
    | t -> Ppx_deriving_biniou_runtime.could_not_convert "Madge.Request.of_biniou" t
  let to_yojson u = `String (Uri.to_string u)
  let of_yojson = function
    | `String s -> Ok (Uri.of_string s) (* FIXME: Uri.of_string fails sometimes I suppose? *)
    | _ -> Error "not a string"
end

type t = {
  meth: meth;
  uri: Uri.t;
  body: string;
}
[@@deriving make, fields, biniou, yojson]
