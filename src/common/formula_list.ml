(* FIXME: move as a sublibrary of Formula *)

open Nes

type 'f predicate =
  | Exists of 'f
  | Forall of 'f
[@@deriving eq, show, yojson, variants]

type 'f t = 'f predicate Formula.t
[@@deriving eq, show, yojson]

let exists' f = Formula.pred (exists f)
let forall' f = Formula.pred (forall f)

let text_formula_converter sub_raw sub_tfc =
  Text_formula_converter.(
    make [
      raw (ok % exists' % sub_raw);
      unary_lift ~name: "exists" (exists, exists_val) ~converter: sub_tfc;
      unary_lift ~name: "forall" (forall, forall_val) ~converter: sub_tfc;
    ]
  )

let optimise sub_optimise =
  Formula.optimise
    ~and_: (fun f1 f2 ->
      match (f1, f2) with
      | (Forall f1, Forall f2) -> some @@ forall (Formula.and_ f1 f2)
      | _ -> None
    )
    ~or_: (fun f1 f2 ->
      match (f1, f2) with
      | (Exists f1, Exists f2) -> some @@ exists (Formula.or_ f1 f2)
      | _ -> None
    )
    (function
      | Exists f -> exists @@ sub_optimise f
      | Forall f -> exists @@ sub_optimise f
    )

let accepts sub_accepts filter values =
  Formula.interpret filter @@ function
    | Exists sub_filter -> Formula.interpret_exists (sub_accepts sub_filter) values
    | Forall sub_filter -> Formula.interpret_forall (sub_accepts sub_filter) values
