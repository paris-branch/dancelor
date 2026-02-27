open Nes

type predicate =
  | Name of Formula_string.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name

let text_formula_converter =
  Text_formula_converter.(
    make
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.text_formula_converter;
      ]
  )

let from_text_formula = Text_formula_converter.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (Text_formula.from_string ?filename input) from_text_formula

let to_text_formula = Text_formula.of_formula text_formula_converter
let to_string = Text_formula.to_string % to_text_formula

let optimise =
  Formula.optimise
    (function
      | Name sfilter -> name @@ Formula_string.optimise sfilter
    )
