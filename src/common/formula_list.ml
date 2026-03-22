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
        lifter ~name: "exists" (exists, exists_val) sub_converter ~down_and: (const2 None);
        lifter ~name: "forall" (forall, forall_val) sub_converter ~down_or: (const2 None);
      ]
      [
        nullary ~name: "empty" empty;
      ]
  )

let accepts sub_accepts filter values =
  Formula.interpret filter @@ function
    | Exists sub_filter -> Formula.interpret_exists (sub_accepts sub_filter) values
    | Forall sub_filter -> Formula.interpret_forall (sub_accepts sub_filter) values
    | Empty -> lwt @@ Formula.interpret_bool (values = [])
