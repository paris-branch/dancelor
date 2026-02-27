open Nes

type predicate =
  | Is of Model_builder.Core.Dance.t Entry.id
  | Name of Formula_string.t
  | Kind of Kind.Dance.Filter.t
  | Devisers of (Model_builder.Core.Person.t, Person.t) Formula_entry.t Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let kind' = Formula.pred % kind
let devisers' = Formula.pred % devisers

let text_formula_converter =
  let unary_lift_devisers ~name =
    Text_formula_converter.unary_lift ~name (devisers, devisers_val) ~converter: (Formula_list.text_formula_converter (Formula_entry.value' % Person.name' % Formula_string.matches') (Formula_entry.text_formula_converter (Person.name' % Formula_string.matches') Person.text_formula_converter));
  in
  Text_formula_converter.(
    make
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.text_formula_converter;
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.text_formula_converter;
        unary_lift_devisers ~name: "devisers";
        unary_lift_devisers ~name: "by";
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
      | (Devisers f1, Devisers f2) -> some @@ devisers (op f1 f2)
      | _ -> None
    )
    (function
      | (Is _ as p) -> p
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
      | Devisers pfilter -> devisers @@ Formula_list.optimise (Formula_entry.optimise Person.optimise) pfilter
    )
