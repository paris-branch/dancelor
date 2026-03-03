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
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.converter;
        unary_lift ~name: "editors" (editors, editors_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Person.converter));
      ]
  )

let optimise =
  Formula.optimise
    ~not_: (function
      | Editors f -> some @@ editors @@ Formula.not f
      | _ -> None
    )
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Editors f1, Editors f2) -> some @@ editors @@ op f1 f2
      | _ -> None
    )
    (function
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Editors pfilter -> editors @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
    )
