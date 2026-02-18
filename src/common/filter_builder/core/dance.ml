open Nes

type predicate =
  | Is of Model_builder.Core.Dance.t Entry.id
  | Name of string
  | Name_matches of string
  | Kind of Kind.Dance.Filter.t
  | Exists_deviser of Person.t (** deviser is defined and passes the filter *)
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let name_matches' = Formula.pred % name_matches
let kind' = Formula.pred % kind
let exists_deviser' = Formula.pred % exists_deviser

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % name_matches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~name: "name-matches" (name_matches, name_matches_val);
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.text_formula_converter;
        unary_lift ~name: "exists-deviser" (exists_deviser, exists_deviser_val) ~converter: Person.text_formula_converter;
        unary_lift ~name: "by" (exists_deviser, exists_deviser_val) ~converter: Person.text_formula_converter;
        (* alias for deviser; FIXME: make this clearer *)
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

let optimise =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
      | (Exists_deviser f1, Exists_deviser f2) -> some @@ exists_deviser (op f1 f2)
      | _ -> None
    )
    ~predicate: (function
      | (Is _ as p) | (Name _ as p) | (Name_matches _ as p) -> p
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
      | Exists_deviser pfilter -> exists_deviser @@ Person.optimise pfilter
    )
