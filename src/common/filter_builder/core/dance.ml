open Nes

type predicate =
  | Name of Formula_string.t
  | Kind of Kind.Dance.Filter.t
  | Devisers of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, ord, show {with_path = false}, yojson]

let name' = Formula.pred % name
let kind' = Formula.pred % kind
let devisers' = Formula.pred % devisers

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    let lifter_devisers ~name =
      Text_formula_converter.lifter ~name (devisers, devisers_val) (Formula_list.converter (Formula_entry.converter_public Person.converter))
    in
    make
      ~debug_name: "dance"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "name" (name, name_val) Formula_string.converter;
        lifter ~name: "kind" (kind, kind_val) Kind.Dance.Filter.converter;
        lifter_devisers ~name: "devisers";
        lifter_devisers ~name: "by";
      ]
      []
      ~compare_predicate
  )
