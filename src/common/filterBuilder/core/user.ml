open Nes

type predicate =
  | Is of ModelBuilder.Core.User.t Entry.id
  | Username of string
  | Username_matches of string
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let username' = Formula.pred % username
let username_matches' = Formula.pred % username_matches

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % username_matches');
        unary_string ~name: "username" (username, username_val);
        unary_string ~name: "username-matches" (username_matches, username_matches_val);
        unary_id ~name: "is" (is, is_val);
      ]
  )

let from_text_formula = TextFormulaConverter.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let optimise =
  Formula.optimise @@ function
    | (Is _ as p) | (Username _ as p) | (Username_matches _ as p) -> p
