open Nes

type predicate =
  | Is of ModelBuilder.Core.Set.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsConceptor of Person.t (** conceptor is defined and passes the filter *)
  | ExistsVersion of Version.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let existsConceptor' = Formula.pred % existsConceptor
let existsVersion' = Formula.pred % existsVersion
let kind' = Formula.pred % kind

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (Result.ok % nameMatches');
        unary_string ~name: "name" (name, unName);
        unary_string ~name: "name-matches" (nameMatches, unNameMatches);
        unary_lift ~name: "exists-conceptor" (existsConceptor, unExistsConceptor) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (existsConceptor, unExistsConceptor) ~converter: Person.text_formula_converter;
        (* alias for exists-conceptor; FIXME: make this clearer *)
        unary_lift ~name: "exists-version" (existsVersion, unExistsVersion) ~converter: Version.text_formula_converter;
        unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Dance.Filter.text_formula_converter;
        unary_string ~name: "is" (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
      ]
  )

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is = is % Entry.slug
let is' = Formula.pred % is

let memVersion = existsVersion % Version.is'
let memVersion' = Formula.pred % memVersion

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (ExistsConceptor f1, ExistsConceptor f2) -> Option.some @@ existsConceptor (op f1 f2)
    | (ExistsVersion f1, ExistsVersion f2) -> Option.some @@ existsVersion (op f1 f2)
    | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
      | ExistsConceptor pfilter -> existsConceptor @@ Person.optimise pfilter
      | ExistsVersion vfilter -> existsVersion @@ Version.optimise vfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
    )
