open Nes

type predicate =
  | Name of Formula_string.t
  | Composers of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
  | Kind of Kind.Base.Filter.t
  | Dances of (Model_builder.Core.Dance.t, Dance.t) Formula_entry.public Formula_list.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, ord, show {with_path = false}, yojson]

let name' = Formula.pred % name
let composers' = Formula.pred % composers
let kind' = Formula.pred % kind
let dances' = Formula.pred % dances

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    let lifter_composers ~name =
      lifter ~name (composers, composers_val) (Formula_list.converter (Formula_entry.converter_public Person.converter))
    in
    make
      ~debug_name: "tune"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "name" (name, name_val) Formula_string.converter;
        lifter_composers ~name: "composers";
        lifter_composers ~name: "by";
        lifter ~name: "kind" (kind, kind_val) Kind.Base.Filter.converter;
        lifter ~name: "dances" (dances, dances_val) (Formula_list.converter (Formula_entry.converter_public Dance.converter));
      ]
      []
      ~compare_predicate
  )
