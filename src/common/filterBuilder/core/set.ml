open Nes

type predicate =
  | Is of ModelBuilder.Core.Set.t Entry.Id.t
  | Name of string
  | NameMatches of string
  | ExistsConceptor of Person.t (** conceptor is defined and passes the filter *)
  | ExistsVersion of Version.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let namematches' = Formula.pred % namematches
let existsconceptor' = Formula.pred % existsconceptor
let existsversion' = Formula.pred % existsversion
let kind' = Formula.pred % kind

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % namematches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (namematches, namematches_val);
        unary_lift ~name: "exists-conceptor" (existsconceptor, existsconceptor_val) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (existsconceptor, existsconceptor_val) ~converter: Person.text_formula_converter;
        (* alias for exists-conceptor; FIXME: make this clearer *)
        unary_lift ~name: "exists-version" (existsversion, existsversion_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.text_formula_converter;
        unary_id ~name: "is" (is, is_val);
      ]
  )

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is = is % Entry.id
let is' = Formula.pred % is

let memversion = existsversion % Version.is'
let memversion' = Formula.pred % memversion

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (ExistsConceptor f1, ExistsConceptor f2) -> some @@ existsconceptor (op f1 f2)
    | (ExistsVersion f1, ExistsVersion f2) -> some @@ existsversion (op f1 f2)
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
      | ExistsConceptor pfilter -> existsconceptor @@ Person.optimise pfilter
      | ExistsVersion vfilter -> existsversion @@ Version.optimise vfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
    )
