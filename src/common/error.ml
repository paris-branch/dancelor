type t =
  | EntityDoesNotExist of string * string
  | StatusViolation of string * string
  | BadQuery of string
  | Unexpected
[@@deriving yojson]

exception Exn of t

let status = function
  | EntityDoesNotExist _ -> `Not_found
  | StatusViolation _ -> `Internal_server_error
  | BadQuery _ -> `Bad_request
  | Unexpected -> `Internal_server_error
