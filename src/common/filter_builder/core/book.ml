open Nes

type predicate =
  | Is of Model_builder.Core.Book.t Entry.Id.t (* FIXME: move to entry-level formulas *)
  | Title of Formula_string.t
  | Versions of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Versions_deep of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Sets of Set.t Formula_list.t
  | Editors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
  | Owners of (Entry.User.t, User.t) Formula_entry.public Formula_list.t (* FIXME: move to entry-level formulas *)
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let versions' = Formula.pred % versions
let sets' = Formula.pred % sets
let versions_deep' = Formula.pred % versions_deep
let editors' = Formula.pred % editors
let owners' = Formula.pred % owners

let converter =
  Text_formula_converter.(
    make
      ~raw: (ok % title' % Formula_string.matches')
      [
        unary_lift ~name: "title" (title, title_val) ~converter: Formula_string.converter;
        unary_lift ~name: "versions" (versions, versions_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Version.converter));
        unary_lift ~name: "sets" (sets, sets_val) ~converter: (Formula_list.converter Set.converter);
        unary_lift ~name: "versions-deep" (versions_deep, versions_deep_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Version.converter));
        unary_lift ~name: "editors" (editors, editors_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Person.converter));
        unary_lift ~name: "owners" (owners, owners_val) ~converter: (Formula_list.converter (Formula_entry.converter_public User.converter));
        unary_id ~name: "is" (is, is_val);
      ]
  )

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
      | Versions vfilter -> versions @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Sets sfilter -> sets @@ Formula_list.optimise Set.optimise sfilter
      | Versions_deep vfilter -> versions_deep @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Editors pfilter -> editors @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
      | Owners lfilter -> owners @@ Formula_list.optimise (Formula_entry.optimise_public User.optimise) lfilter
    )
