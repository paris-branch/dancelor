open Nes

type predicate =
  | Is of Model_builder.Core.Set.t Entry.Id.t
  | Name of string
  | Name_matches of string
  | Conceptors of Person.t Formula_list.t
  | Versions of Version.t Formula_list.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let name_matches' = Formula.pred % name_matches
let conceptors' = Formula.pred % conceptors
let versions' = Formula.pred % versions
let kind' = Formula.pred % kind

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % name_matches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (name_matches, name_matches_val);
        unary_lift ~name: "conceptors" (conceptors, conceptors_val) ~converter: (Formula_list.text_formula_converter Person.name_matches' Person.text_formula_converter);
        unary_lift ~name: "by" (conceptors, conceptors_val) ~converter: (Formula_list.text_formula_converter Person.name_matches' Person.text_formula_converter);
        (* alias for exists-conceptor; FIXME: make this clearer *)
        unary_lift ~name: "versions" (versions, versions_val) ~converter: (Formula_list.text_formula_converter (Version.tune' % Tune.name_matches') Version.text_formula_converter);
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

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Conceptors f1, Conceptors f2) -> some @@ conceptors (op f1 f2)
    | (Versions f1, Versions f2) -> some @@ versions (op f1 f2)
    | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Name _ as p) | (Name_matches _ as p) -> p
      | Conceptors pfilter -> conceptors @@ Formula_list.optimise Person.optimise pfilter
      | Versions vfilter -> versions @@ Formula_list.optimise Version.optimise vfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
    )
