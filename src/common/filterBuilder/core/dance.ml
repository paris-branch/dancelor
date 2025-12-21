open Nes

type predicate =
  | Is of ModelBuilder.Core.Dance.t Entry.Id.t
  | Name of string
  | NameMatches of string
  | Kind of Kind.Dance.Filter.t
  | ExistsDeviser of Person.t (** deviser is defined and passes the filter *)
[@@deriving eq, show {with_path = false}, biniou, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, biniou, yojson]

let name' = Formula.pred % name
let namematches' = Formula.pred % namematches
let kind' = Formula.pred % kind
let existsdeviser' = Formula.pred % existsdeviser

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % namematches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (namematches, namematches_val);
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.text_formula_converter;
        unary_lift ~name: "exists-deviser" (existsdeviser, existsdeviser_val) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (existsdeviser, existsdeviser_val) ~converter: Person.text_formula_converter;
        (* alias for deviser; FIXME: make this clearer *)
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

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | (ExistsDeviser f1, ExistsDeviser f2) -> some @@ existsdeviser (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
      | ExistsDeviser pfilter -> existsdeviser @@ Person.optimise pfilter
    )
