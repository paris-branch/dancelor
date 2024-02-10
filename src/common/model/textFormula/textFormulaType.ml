open Nes
open Formula

type t = predicate Formula.t

and predicate =
  | Raw of string
  | Nullary of string
  | Unary of string * t
[@@deriving show]

let raw s = Raw s
let nullary p = Nullary p
let unary p e = Unary (p, e)

let raw' = pred % raw
let nullary' = pred % nullary
let unary' p e = pred (unary p e)

let gen_predicate = let open QCheck.Gen in
  frequency
    [(1, (raw <$> string_printable));
     (1, (nullary <$> string_size ~gen:(char_range 'a' 'z') (int_range 1 10)))]
(* (1, (unary <$> string_size ~gen:(char_range 'a' 'z') (int_range 1 10)) <*> Formula.gen gen_predicate))] *)
(* FIXME: reactivate last case *)

let gen = Formula.gen gen_predicate

let shrink' _ = QCheck.Iter.empty

(** Characters that require escaping. *)
let special_chars = [':'; '"'; '('; ')'; '\\'; ' ']
let is_special = Fun.flip List.mem special_chars
