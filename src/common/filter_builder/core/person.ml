open Nes

type predicate =
  | Is of Model_builder.Core.Person.t Entry.id
  | Name of Formula_string.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % name' % Formula_string.matches');
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.text_formula_converter;
        unary_id ~name: "is" (is, is_val);
      ]
  )

let from_text_formula = Text_formula_converter.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (Text_formula.from_string ?filename input) from_text_formula

let to_text_formula = Text_formula.of_formula text_formula_converter
let to_string = Text_formula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let optimise =
  Formula.optimise
    ~binop: (fun _ _ _ -> None)
    ~predicate: (function
      | (Is _ as p) -> p
      | Name sfilter -> name @@ Formula_string.optimise sfilter
    )
