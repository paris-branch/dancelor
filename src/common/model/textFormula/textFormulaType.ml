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

let gen_predicate_name =
  let open QCheck.Gen in
  fix
    (fun self () ->
       let* name = string_size ~gen:(char_range 'a' 'z') (int_range 1 10) in
       if name = "or" || name = "and" || name = "not" then
         self ()
       else
         pure name)
    ()

let gen_predicate =
  let open QCheck.Gen in
  fix
    (fun self () ->
       oneof [
         (raw <$> string_printable);
         (nullary <$> gen_predicate_name);
         (unary <$> gen_predicate_name <*> Formula.gen (self ()));
       ]
    )
    ()

let gen = Formula.gen gen_predicate

let shrink' _ = QCheck.Iter.empty

(** Characters that require escaping. *)
let special_chars = [':'; '"'; '('; ')'; '\\'; ' ']
let is_special = Fun.flip List.mem special_chars
