open Protocol_conv_jsonm

type t =
  | EntityDoesNotExist of string * string
[@@deriving protocol ~driver:(module Jsonm)]

exception Exn of t

let status = function
  | EntityDoesNotExist _ -> `Not_found
