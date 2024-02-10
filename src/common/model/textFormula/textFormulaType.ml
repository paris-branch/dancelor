open Formula

type t = predicate Formula.t

and predicate =
  | Raw of string
  | Nullary of string
  | Unary of string * t
[@@deriving show]

let raw s = pred (Raw s)
let nullary p = pred (Nullary p)
let unary p e = pred (Unary (p, e))
