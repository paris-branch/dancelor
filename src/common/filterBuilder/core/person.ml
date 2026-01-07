open Nes

type predicate =
  | Is of ModelBuilder.Core.Person.t Entry.id
  | Name of string
  | NameMatches of string
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % namematches

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % nameMatches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (namematches, namematches_val);
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
  Formula.optimise @@ function
    | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
