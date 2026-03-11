(* FIXME: move as a sublibrary of Formula *)

open Nes

type 'f predicate =
  | Empty
  | Exists of 'f
  | Forall of 'f
[@@deriving eq, show, yojson, variants]

type 'f t = 'f predicate Formula.t
[@@deriving eq, show, yojson]

let empty' = Formula.pred empty
let exists' f = Formula.pred (exists f)
let forall' f = Formula.pred (forall f)

let converter sub_converter =
  Text_formula_converter.(
    make
      ~debug_name: (spf "list(%s)" @@ Text_formula_converter.debug_name sub_converter)
      ~debug_print: (fun fmt _ -> fpf fmt "<opaque list>")
      ~raw: (Result.map exists' % raw sub_converter)
      ~lifters: [
        lifter ~name: "exists" (exists, exists_val) sub_converter;
        lifter ~name: "forall" (forall, forall_val) sub_converter;
      ]
      [
        nullary ~name: "empty" empty;
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
      | Empty as p -> p
    )

let accepts sub_accepts filter values =
  Formula.interpret filter @@ function
    | Exists sub_filter -> Formula.interpret_exists (sub_accepts sub_filter) values
    | Forall sub_filter -> Formula.interpret_forall (sub_accepts sub_filter) values
    | Empty -> lwt @@ Formula.interpret_bool (values = [])
