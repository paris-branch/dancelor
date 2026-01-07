open Nes

type predicate =
  | Is of Model_builder.Core.Set.t Entry.Id.t
  | Name of string
  | Name_matches of string
  | Exists_conceptor of Person.t (** conceptor is defined and passes the filter *)
  | Exists_version of Version.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let name_matches' = Formula.pred % name_matches
let exists_conceptor' = Formula.pred % exists_conceptor
let exists_version' = Formula.pred % exists_version
let kind' = Formula.pred % kind

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % name_matches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (name_matches, name_matches_val);
        unary_lift ~name: "exists-conceptor" (exists_conceptor, exists_conceptor_val) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (exists_conceptor, exists_conceptor_val) ~converter: Person.text_formula_converter;
        (* alias for exists-conceptor; FIXME: make this clearer *)
        unary_lift ~name: "exists-version" (exists_version, exists_version_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.text_formula_converter;
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

let memversion x = exists_version @@ Version.is' x
let memversion' x = Formula.pred @@ memversion x

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Exists_conceptor f1, Exists_conceptor f2) -> some @@ exists_conceptor (op f1 f2)
    | (Exists_version f1, Exists_version f2) -> some @@ exists_version (op f1 f2)
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (Name_matches _ as p) -> p
      | Exists_conceptor pfilter -> exists_conceptor @@ Person.optimise pfilter
      | Exists_version vfilter -> exists_version @@ Version.optimise vfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
    )
