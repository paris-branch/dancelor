open Nes

type predicate =
  | Title of Formula_string.t
  | Versions of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Versions_deep of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Sets of (Model_builder.Core.Set.t, Set.t) Formula_entry.private_ Formula_list.t
  | Editors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let versions' = Formula.pred % versions
let sets' = Formula.pred % sets
let versions_deep' = Formula.pred % versions_deep
let editors' = Formula.pred % editors

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "book"
      ~debug_print: pp_predicate
      ~raw: (ok % title' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "title" (title, title_val) Formula_string.converter;
        lifter ~name: "versions" (versions, versions_val) (Formula_list.converter (Formula_entry.converter_public Version.converter));
        lifter ~name: "sets" (sets, sets_val) (Formula_list.converter (Formula_entry.converter_private Set.converter));
        lifter ~name: "versions-deep" (versions_deep, versions_deep_val) (Formula_list.converter (Formula_entry.converter_public Version.converter));
        lifter ~name: "editors" (editors, editors_val) (Formula_list.converter (Formula_entry.converter_public Person.converter));
      ]
      []
  )

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
      | Title sfilter -> title @@ Formula_string.optimise sfilter
      | Versions vfilter -> versions @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Sets sfilter -> sets @@ Formula_list.optimise (Formula_entry.optimise_private Set.optimise) sfilter
      | Versions_deep vfilter -> versions_deep @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Editors pfilter -> editors @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
    )
