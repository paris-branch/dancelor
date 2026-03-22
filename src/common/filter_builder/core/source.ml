open Nes

type predicate =
  | Name of Formula_string.t
  | Editors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let editors' = Formula.pred % editors

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "source"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "name" (name, name_val) Formula_string.converter;
        lifter ~name: "editors" (editors, editors_val) (Formula_list.converter (Formula_entry.converter_public Person.converter));
      ]
      []
  )
