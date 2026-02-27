open Nes

type predicate =
  | Is of Model_builder.Core.Tune.t Entry.Id.t
  | Name of Formula_string.t
  | Composers of (Model_builder.Core.Person.t, Person.t) Formula_entry.t Formula_list.t
  | Kind of Kind.Base.Filter.t
  | Dances of Dance.t Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let composers' = Formula.pred % composers
let kind' = Formula.pred % kind
let dances' = Formula.pred % dances

let text_formula_converter =
  let unary_lift_composers ~name =
    Text_formula_converter.unary_lift ~name (composers, composers_val) ~converter: (Formula_list.text_formula_converter (Formula_entry.value' % Person.name' % Formula_string.matches') (Formula_entry.text_formula_converter (Person.name' % Formula_string.matches') Person.text_formula_converter));
  in
  Text_formula_converter.(
    make
      [
        raw (ok % name' % Formula_string.matches');
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.text_formula_converter;
        unary_lift_composers ~name: "composers";
        unary_lift_composers ~name: "by";
        (* alias for composers *)
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Base.Filter.text_formula_converter;
        unary_lift ~name: "dances" (dances, dances_val) ~converter: (Formula_list.text_formula_converter (Dance.name' % Formula_string.matches') Dance.text_formula_converter);
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
    (function
      | (Is _ as p) -> p
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Composers pfilter -> composers @@ Formula_list.optimise (Formula_entry.optimise Person.optimise) pfilter
      | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
      | Dances dfilter -> dances @@ Formula_list.optimise Dance.optimise dfilter
    )
