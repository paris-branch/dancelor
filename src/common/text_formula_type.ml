open Nes
open Formula

type t = predicate Formula.t

and predicate =
  | Raw of string
  | Nullary of string
  | Unary of string * t
[@@deriving eq, show]

let raw s = Raw s
let nullary p = Nullary p
let unary p e = Unary (p, e)

let raw' = pred % raw
let nullary' = pred % nullary
let unary' p e = pred (unary p e)

(** Characters that require escaping. *)
let special_chars = [':'; '"'; '('; ')'; '\\'; ' ']
let is_special = flip List.mem special_chars
