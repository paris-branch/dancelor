type t =
  | EntityDoesNotExist of string * string
[@@deriving yojson]

exception Exn of t

let status = function
  | EntityDoesNotExist _ -> `Not_found
