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
let namematches' = Formula.pred % namematches
let existscomposer' = Formula.pred % existscomposer
let kind' = Formula.pred % kind
let existsdance' = Formula.pred % existsdance

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % namematches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~wrap_back: Never ~name: "name-matches" (namematches, namematches_val);
        unary_lift ~name: "exists-composer" (existscomposer, existscomposer_val) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (existscomposer, existscomposer_val) ~converter: Person.text_formula_converter;
        (* alias for exists-composer; FIXME: make this clearer *)
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Base.Filter.text_formula_converter;
        unary_lift ~name: "exists-dance" (existsdance, existsdance_val) ~converter: Dance.text_formula_converter;
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

let existscomposeris = existscomposer % Person.is'
let existscomposeris' = Formula.pred % existscomposeris

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (ExistsComposer f1, ExistsComposer f2) -> some @@ existscomposer (op f1 f2)
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | (ExistsDance f1, ExistsDance f2) -> some @@ existsdance (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (NameMatches _ as p) -> p
      | ExistsComposer pfilter -> existscomposer @@ Person.optimise pfilter
      | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
      | ExistsDance dfilter -> existsdance @@ Dance.optimise dfilter
    )
