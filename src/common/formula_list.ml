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

let optimise sub_optimise =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      (* FIXME: this is simply not true; And does not commute with Exists *)
      | (Exists f1, Exists f2) -> some @@ exists (op f1 f2)
    )
    ~predicate: (function
      | Exists f -> exists @@ sub_optimise f
    )

let accepts sub_accepts filter values =
  Formula.interpret filter @@ function
    | Exists sub_filter ->
      Formula.interpret_exists (sub_accepts sub_filter) values
