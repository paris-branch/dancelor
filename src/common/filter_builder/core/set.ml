open Nes

type predicate =
  | Is of Model_builder.Core.Set.t Entry.Id.t (* FIXME: move to entry-level formulas *)
  | Name of Formula_string.t
  | Conceptors of (Model_builder.Core.Person.t, Person.t) Formula_entry.t Formula_list.t
  | Versions of Version.t Formula_list.t
  | Kind of Kind.Dance.Filter.t
  | Owners of User.t Formula_list.t (* FIXME: move to entry-level formulas *)
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let conceptors' = Formula.pred % conceptors
let versions' = Formula.pred % versions
let kind' = Formula.pred % kind
let owners' = Formula.pred % owners

let text_formula_converter =
  let unary_lift_conceptors ~name =
    Text_formula_converter.unary_lift
      ~name
      (conceptors, conceptors_val)
      ~converter: (
        Formula_list.text_formula_converter
          (Formula_entry.value' % Person.name' % Formula_string.matches')
          (Formula_entry.text_formula_converter (Person.name' % Formula_string.matches') Person.text_formula_converter)
      )
  in
  Text_formula_converter.(
    make
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.text_formula_converter;
        unary_lift_conceptors ~name: "conceptors";
        unary_lift_conceptors ~name: "by";
        unary_lift ~name: "versions" (versions, versions_val) ~converter: (Formula_list.text_formula_converter (Version.tune' % Tune.name' % Formula_string.matches') Version.text_formula_converter);
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.text_formula_converter;
        unary_lift ~name: "owners" (owners, owners_val) ~converter: (Formula_list.text_formula_converter (User.username' % Formula_string.matches') User.text_formula_converter);
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
      | (Conceptors f1, Conceptors f2) -> some @@ conceptors (op f1 f2)
      | (Versions f1, Versions f2) -> some @@ versions (op f1 f2)
      | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
      | _ -> None
    )
    (function
      | (Is _ as p) -> p
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Conceptors pfilter -> conceptors @@ Formula_list.optimise (Formula_entry.optimise Person.optimise) pfilter
      | Versions vfilter -> versions @@ Formula_list.optimise Version.optimise vfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
      | Owners lfilter -> owners @@ Formula_list.optimise User.optimise lfilter
    )
