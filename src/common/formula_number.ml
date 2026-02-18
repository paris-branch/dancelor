(* FIXME: move as a sublibrary of Formula *)

open Nes

type predicate =
  | Eq of int
  | Ne of int
  | Gt of int
  | Lt of int
  | Ge of int
  | Le of int
[@@deriving eq, show, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show, yojson]

let eq' f = Formula.pred (eq f)
let ne' f = Formula.pred (ne f)
let gt' f = Formula.pred (gt f)
let lt' f = Formula.pred (lt f)
let ge' f = Formula.pred (ge f)
let le' f = Formula.pred (le f)

let text_formula_converter =
  Text_formula_converter.(
    make [
      (* raw (ok % eq'); *)
      unary_int ~name: "eq" (eq, eq_val);
      unary_int ~name: "ne" (ne, ne_val);
      unary_int ~name: "gt" (gt, gt_val);
      unary_int ~name: "lt" (lt, lt_val);
      unary_int ~name: "ge" (ge, ge_val);
      unary_int ~name: "le" (le, le_val);
    ]
  )

let optimise f =
  Formula.optimise
    ~binop: (fun _ _ _ -> None)
    ~predicate: (fun p -> p)
    f

let accepts filter value =
  Formula.interpret filter @@ function
    | Eq n -> lwt @@ Formula.interpret_bool (value = n)
    | Ne n -> lwt @@ Formula.interpret_bool (value <> n)
    | Gt n -> lwt @@ Formula.interpret_bool (value > n)
    | Lt n -> lwt @@ Formula.interpret_bool (value < n)
    | Ge n -> lwt @@ Formula.interpret_bool (value >= n)
    | Le n -> lwt @@ Formula.interpret_bool (value <= n)
