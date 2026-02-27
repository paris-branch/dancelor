open Nes

type predicate =
  | Is of Model_builder.Core.Book.t Entry.Id.t (* FIXME: move to entry-level formulas *)
  | Title of Formula_string.t
  | Versions of Version.t Formula_list.t
  | Sets of Set.t Formula_list.t
  | Versions_deep of Version.t Formula_list.t
  | Editors of (Model_builder.Core.Person.t, Person.t) Formula_entry.t Formula_list.t
  | Owners of User.t Formula_list.t (* FIXME: move to entry-level formulas *)
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let versions' = Formula.pred % versions
let sets' = Formula.pred % sets
let versions_deep' = Formula.pred % versions_deep
let editors' = Formula.pred % editors
let owners' = Formula.pred % owners

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % title' % Formula_string.matches');
        unary_lift ~name: "title" (title, title_val) ~converter: Formula_string.text_formula_converter;
        unary_lift ~name: "versions" (versions, versions_val) ~converter: (Formula_list.text_formula_converter (Version.tune' % Tune.name' % Formula_string.matches') Version.text_formula_converter);
        unary_lift ~name: "sets" (sets, sets_val) ~converter: (Formula_list.text_formula_converter (Set.name' % Formula_string.matches') Set.text_formula_converter);
        unary_lift ~name: "versions-deep" (versions_deep, versions_deep_val) ~converter: (Formula_list.text_formula_converter (Version.tune' % Tune.name' % Formula_string.matches') Version.text_formula_converter);
        unary_lift ~name: "editors" (editors, editors_val) ~converter: (Formula_list.text_formula_converter (Formula_entry.value' % Person.name' % Formula_string.matches') (Formula_entry.text_formula_converter (Person.name' % Formula_string.matches') Person.text_formula_converter));
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
      | (Versions f1, Versions f2) -> some @@ versions (op f1 f2)
      | (Sets f1, Sets f2) -> some @@ sets (op f1 f2)
      | (Versions_deep f1, Versions_deep f2) -> some @@ versions_deep (op f1 f2)
      | _ -> None
    )
    (function
      | (Is _ as p) -> p
      | Title sfilter -> title @@ Formula_string.optimise sfilter
      | Versions vfilter -> versions @@ Formula_list.optimise Version.optimise vfilter
      | Sets sfilter -> sets @@ Formula_list.optimise Set.optimise sfilter
      | Versions_deep vfilter -> versions_deep @@ Formula_list.optimise Version.optimise vfilter
      | Editors pfilter -> editors @@ Formula_list.optimise (Formula_entry.optimise Person.optimise) pfilter
      | Owners lfilter -> owners @@ Formula_list.optimise User.optimise lfilter
    )
