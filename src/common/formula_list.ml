(* FIXME: move as a sublibrary of Formula *)

open Nes

type 'f predicate =
  | Empty
  | Exists of 'f
  | Forall of 'f
[@@deriving eq, show {with_path = false}, yojson, variants]

type 'f t = 'f predicate Formula.t
[@@deriving eq, show, yojson]

let empty' = Formula.pred empty
let exists' f = Formula.pred (exists f)
let forall' f = Formula.pred (forall f)

let converter sub_converter =
  Text_formula_converter.(
    make
      ~raw: (Result.map exists' % raw sub_converter)
      [
        nullary ~name: "empty" empty;
        unary_lift ~name: "exists" (exists, exists_val) ~converter: sub_converter;
        unary_lift ~name: "forall" (forall, forall_val) ~converter: sub_converter;
      ]
  )

let optimise sub_optimise =
  Formula.optimise
    ~up_true: (fun {is_true} ->
      function
        | Exists f when is_true f -> some @@ Formula.not @@ empty'
        | Forall f when is_true f -> some Formula.true_
        | _ -> None
    )
    ~up_false: (fun {is_false} ->
      function
        | Exists f when is_false f -> some Formula.false_
        | Forall f when is_false f -> some empty'
        | _ -> None
    )
    ~not_: (function
      | Exists f -> some @@ forall' (Formula.not f)
      | Forall f -> some @@ exists' (Formula.not f)
      | _ -> None
    )
    ~and_: (fun f1 f2 ->
      match (f1, f2) with
      | (Forall f1, Forall f2) -> some @@ forall (Formula.and_ f1 f2)
      | (Empty, Forall _) | (Forall _, Empty) -> some Empty
      (* | (Empty, Exists _) | (Exists _, Empty) -> some False *)
      | _ -> None
    )
    ~or_: (fun f1 f2 ->
      match (f1, f2) with
      | (Exists f1, Exists f2) -> some @@ exists (Formula.or_ f1 f2)
      | _ -> None
    )
    (function
      | Exists f -> exists @@ sub_optimise f
      | Forall f -> forall @@ sub_optimise f
      | Empty as p -> p
    )

let accepts sub_accepts filter values =
  Formula.interpret filter @@ function
    | Exists sub_filter -> Formula.interpret_exists (sub_accepts sub_filter) values
    | Forall sub_filter -> Formula.interpret_forall (sub_accepts sub_filter) values
    | Empty -> lwt @@ Formula.interpret_bool (values = [])
