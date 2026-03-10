(* FIXME: move as a sublibrary of Formula *)

open Nes

type predicate =
  | Eq of string
  | Matches of string
[@@deriving eq, show, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show, yojson]

let eq' s = Formula.pred (eq s)
let matches' s = Formula.pred (matches s)

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "string"
      ~debug_print: pp_predicate
      ~raw: (ok % matches')
      [
        unary_string ~name: "eq" (eq, eq_val);
        unary_string ~name: "matches" (matches, matches_val);
      ]
  )

let optimise f = Formula.optimise Fun.id f

let accepts filter value =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function
    | Eq needle ->
      lwt @@ String.proximity ~char_equal needle value
    | Matches needle ->
      lwt @@ String.inclusion_proximity ~char_equal ~needle value
