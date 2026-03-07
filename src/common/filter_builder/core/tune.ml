open Nes

type predicate =
  | Name of Formula_string.t
  | Composers of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
  | Kind of Kind.Base.Filter.t
  | Dances of (Model_builder.Core.Dance.t, Dance.t) Formula_entry.public Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let composers' = Formula.pred % composers
let kind' = Formula.pred % kind
let dances' = Formula.pred % dances

let converter =
  let unary_lift_composers ~name =
    Text_formula_converter.unary_lift ~name (composers, composers_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Person.converter));
  in
  Text_formula_converter.(
    make
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.converter;
        unary_lift_composers ~name: "composers";
        unary_lift_composers ~name: "by";
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Base.Filter.converter;
        unary_lift ~name: "dances" (dances, dances_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Dance.converter));
      ]
  )

let optimise =
  Formula.optimise
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Composers f1, Composers f2) -> some @@ Composers (op f1 f2)
      | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
      | (Dances f1, Dances f2) -> some @@ Dances (op f1 f2)
      | _ -> None
    )
    (function
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Composers pfilter -> composers @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
      | Kind kfilter -> kind @@ Kind.Base.Filter.optimise kfilter
      | Dances dfilter -> dances @@ Formula_list.optimise (Formula_entry.optimise_public Dance.optimise) dfilter
    )
