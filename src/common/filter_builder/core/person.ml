open Nes

type predicate =
  | Name of Formula_string.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "person"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.converter;
      ]
  )

let optimise =
  Formula.optimise
    (function
      | Name sfilter -> name @@ Formula_string.optimise sfilter
    )
