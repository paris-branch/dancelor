(* FIXME: move as a sublibrary of Formula *)

open Nes

type 'f predicate =
  | Exists of 'f
[@@deriving eq, show, yojson, variants]

type 'f t = 'f predicate Formula.t
[@@deriving eq, show, yojson]

let exists' f = Formula.pred (exists f)

let text_formula_converter sub_raw sub_tfc =
  Text_formula_converter.(
    make [
      raw (ok % exists' % sub_raw);
      unary_lift ~name: "exists" (exists, exists_val) ~converter: sub_tfc;
    ]
  )

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise sub_optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Exists f1, Exists f2) -> some @@ exists (op f1 f2)
  (* | _ -> None *)
  in Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | Exists f -> exists @@ sub_optimise f
    )

let accepts sub_accepts filter values =
  Formula.interpret filter @@ function
    | Exists sub_filter ->
      Formula.interpret_exists (sub_accepts sub_filter) values
