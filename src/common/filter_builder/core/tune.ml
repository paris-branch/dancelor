open Nes

type predicate =
  | Is of Model_builder.Core.Tune.t Entry.Id.t
  | Name of string
  | Name_matches of string
  | Composers of Person.t Formula_list.t
  | Kind of Kind.Base.Filter.t
  | Dances of Dance.t Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let name_matches' = Formula.pred % name_matches
let composers' = Formula.pred % composers
let kind' = Formula.pred % kind
let dances' = Formula.pred % dances

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % name_matches');
        unary_string ~name: "name" (name, name_val);
        unary_string ~wrap_back: Never ~name: "name-matches" (name_matches, name_matches_val);
        unary_lift ~name: "composers" (composers, composers_val) ~converter: (Formula_list.text_formula_converter Person.name_matches' Person.text_formula_converter);
        unary_lift ~name: "by" (composers, composers_val) ~converter: (Formula_list.text_formula_converter Person.name_matches' Person.text_formula_converter);
        (* alias for composers; FIXME: make this clearer *)
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Base.Filter.text_formula_converter;
        unary_lift ~name: "dances" (dances, dances_val) ~converter: (Formula_list.text_formula_converter Dance.name_matches' Dance.text_formula_converter);
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
      | (Composers f1, Composers f2) -> some @@ Composers (op f1 f2)
      | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
      | (Dances f1, Dances f2) -> some @@ Dances (op f1 f2)
      | _ -> None
    )
    ~predicate: (function
      | (Is _ as p) | (Name _ as p) | (Name_matches _ as p) -> p
      | Composers pfilter -> composers @@ Formula_list.optimise Person.optimise pfilter
      | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
      | Dances dfilter -> dances @@ Formula_list.optimise Dance.optimise dfilter
    )
