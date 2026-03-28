open Nes

type predicate =
  | Name of Formula_string.t
  | Conceptors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
  | Versions of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, ord, show {with_path = false}, yojson]

let name' = Formula.pred % name
let conceptors' = Formula.pred % conceptors
let versions' = Formula.pred % versions
let kind' = Formula.pred % kind

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    let lifter_conceptors ~name =
      lifter ~name (conceptors, conceptors_val) (Formula_list.converter (Formula_entry.converter_public Person.converter))
    in
    make
      ~debug_name: "set"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "name" (name, name_val) Formula_string.converter;
        lifter_conceptors ~name: "conceptors";
        lifter_conceptors ~name: "by";
        lifter ~name: "versions" (versions, versions_val) (Formula_list.converter (Formula_entry.converter_public Version.converter));
        lifter ~name: "kind" (kind, kind_val) Kind.Dance.Filter.converter;
      ]
      []
      ~compare_predicate
  )
