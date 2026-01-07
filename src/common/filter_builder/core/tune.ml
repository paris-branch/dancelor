open Nes

type predicate =
  | Is of Model_builder.Core.Tune.t Entry.Id.t
  | Name of string
  | Name_matches of string
  | Exists_composer of Person.t (** one of the composers of the list passes the filter *)
  | Kind of Kind.Base.Filter.t
  | Exists_dance of Dance.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let name_matches' = Formula.pred % name_matches
let exists_composer' = Formula.pred % exists_composer
let kind' = Formula.pred % kind
let exists_dance' = Formula.pred % exists_dance

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % name_matches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~wrap_back: Never ~name: "name-matches" (name_matches, name_matches_val);
        unary_lift ~name: "exists-composer" (exists_composer, exists_composer_val) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (exists_composer, exists_composer_val) ~converter: Person.text_formula_converter;
        (* alias for exists-composer; FIXME: make this clearer *)
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Base.Filter.text_formula_converter;
        unary_lift ~name: "exists-dance" (exists_dance, exists_dance_val) ~converter: Dance.text_formula_converter;
        unary_id ~name: "is" (is, is_val);
      ]
  )

let from_text_formula = Text_formula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (Text_formula.from_string ?filename input) from_text_formula

let to_text_formula = Text_formula.of_formula text_formula_converter
let to_string = Text_formula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let exists_composer_is x = exists_composer @@ Person.is' x
let exists_composer_is' x = Formula.pred @@ exists_composer_is x

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Exists_composer f1, Exists_composer f2) -> some @@ exists_composer (op f1 f2)
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | (Exists_dance f1, Exists_dance f2) -> some @@ exists_dance (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (Name_matches _ as p) -> p
      | Exists_composer pfilter -> exists_composer @@ Person.optimise pfilter
      | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
      | Exists_dance dfilter -> exists_dance @@ Dance.optimise dfilter
    )
