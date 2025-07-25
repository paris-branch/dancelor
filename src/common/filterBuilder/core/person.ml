open Nes

type predicate =
  | Is of ModelBuilder.Core.Person.t Entry.Id.t
  | Name of string
  | NameMatches of string
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % nameMatches');
        unary_string ~name: "name" (name, unName);
        unary_string ~name: "name-matches" (nameMatches, unNameMatches);
        unary_id ~name: "is" (is, unIs);
      ]
  )

let from_text_formula = TextFormulaConverter.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is = is % Entry.id
let is' = Formula.pred % is

let optimise =
  Formula.optimise @@ function
    | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
