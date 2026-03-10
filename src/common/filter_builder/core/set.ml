open Nes

type predicate =
  | Name of Formula_string.t
  | Conceptors of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
  | Versions of (Model_builder.Core.Version.t, Version.t) Formula_entry.public Formula_list.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let conceptors' = Formula.pred % conceptors
let versions' = Formula.pred % versions
let kind' = Formula.pred % kind

let converter =
  let unary_lift_conceptors ~name =
    Text_formula_converter.unary_lift ~name (conceptors, conceptors_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Person.converter))
  in
  Text_formula_converter.(
    make
      ~debug_name: "set"
      ~debug_print: pp_predicate
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.converter;
        unary_lift_conceptors ~name: "conceptors";
        unary_lift_conceptors ~name: "by";
        unary_lift ~name: "versions" (versions, versions_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Version.converter));
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.converter;
      ]
  )

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
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Conceptors pfilter -> conceptors @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
      | Versions vfilter -> versions @@ Formula_list.optimise (Formula_entry.optimise_public Version.optimise) vfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
    )
