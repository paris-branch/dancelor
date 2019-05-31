type t =
  | EntityDoesNotExist of string * string
  | BadQuery of string
  | Unexpected
[@@deriving yojson]

exception Exn of t

let status = function
  | EntityDoesNotExist _ -> `Not_found
  | BadQuery _ -> `Bad_request
  | Unexpected -> `Internal_server_error
