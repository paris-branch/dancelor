open Nes

type predicate =
  | Is of ModelBuilder.Core.Tune.t Entry.Id.t
  | Name of string
  | NameMatches of string
  | ExistsComposer of Person.t (** one of the composers of the list passes the filter *)
  | Kind of Kind.Base.Filter.t
  | ExistsDance of Dance.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let existsComposer' = Formula.pred % existsComposer
let kind' = Formula.pred % kind
let existsDance' = Formula.pred % existsDance

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % nameMatches');
        unary_string ~name: "name" (name, unName);
        unary_string ~wrap_back: Never ~name: "name-matches" (nameMatches, unNameMatches);
        unary_lift ~name: "exists-composer" (existsComposer, unExistsComposer) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (existsComposer, unExistsComposer) ~converter: Person.text_formula_converter;
        (* alias for exists-composer; FIXME: make this clearer *)
        unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Base.Filter.text_formula_converter;
        unary_lift ~name: "exists-dance" (existsDance, unExistsDance) ~converter: Dance.text_formula_converter;
        unary_id ~name: "is" (is, unIs);
      ]
  )

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is = is % Entry.id
let is' = Formula.pred % is

let existsComposerIs = existsComposer % Person.is'
let existsComposerIs' = Formula.pred % existsComposerIs

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (ExistsComposer f1, ExistsComposer f2) -> some @@ existsComposer (op f1 f2)
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | (ExistsDance f1, ExistsDance f2) -> some @@ existsDance (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
      | ExistsComposer pfilter -> existsComposer @@ Person.optimise pfilter
      | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
      | ExistsDance dfilter -> existsDance @@ Dance.optimise dfilter
    )
