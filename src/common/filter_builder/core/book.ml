open Nes

type predicate =
  | Name of Formula_string.t
  | Versions of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Versions_deep of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Sets of (Model_builder.Core.Set.t, Set.t) Formula_entry.private_ Formula_list.t
  | Editors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, ord, show {with_path = false}, yojson]

let name' = Formula.pred % name
let versions' = Formula.pred % versions
let sets' = Formula.pred % sets
let versions_deep' = Formula.pred % versions_deep
let editors' = Formula.pred % editors

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    make
      ~debug_name: "book"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "name" (name, name_val) Formula_string.converter;
        lifter ~name: "versions" (versions, versions_val) (Formula_list.converter (Formula_entry.converter_public Version.converter));
        lifter ~name: "sets" (sets, sets_val) (Formula_list.converter (Formula_entry.converter_private Set.converter));
        lifter ~name: "versions-deep" (versions_deep, versions_deep_val) (Formula_list.converter (Formula_entry.converter_public Version.converter));
        lifter ~name: "editors" (editors, editors_val) (Formula_list.converter (Formula_entry.converter_public Person.converter));
      ]
      []
      ~compare_predicate
  )
