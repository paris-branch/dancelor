open Nes

type predicate =
  | Is of ModelBuilder.Core.Source.t Entry.Id.t
  | Name of string
  | NameMatches of string
  | ExistsEditor of Person.t
[@@deriving eq, show {with_path = false}, biniou, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, biniou, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % namematches
let existseditor' = Formula.pred % existseditor

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % nameMatches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (namematches, namematches_val);
        unary_id ~name: "is" (is, is_val);
        unary_lift ~name: "exists-editor" (existseditor, existseditor_val) ~converter: Person.text_formula_converter;
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
    | ExistsEditor pfilter -> existseditor @@ Person.optimise pfilter
