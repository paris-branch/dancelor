open Nes

type predicate =
  | Name of Formula_string.t
  | Kind of Kind.Dance.Filter.t
  | Devisers of (Model_builder.Core.Person.t, Person.t) Formula_entry.public Formula_list.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let kind' = Formula.pred % kind
let devisers' = Formula.pred % devisers

let converter =
  let unary_lift_devisers ~name =
    Text_formula_converter.unary_lift ~name (devisers, devisers_val) ~converter: (Formula_list.converter (Formula_entry.converter_public Person.converter));
  in
  Text_formula_converter.(
    make
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.converter;
        unary_lift ~name: "kind" (kind, kind_val) ~converter: Kind.Dance.Filter.converter;
        unary_lift_devisers ~name: "devisers";
        unary_lift_devisers ~name: "by";
      ]
  )

let optimise =
  Formula.optimise
    ~up: (fun {is_tf} ->
      function
        | Name f -> is_tf f
        | Kind f -> is_tf f
        | Devisers f -> is_tf f
    )
    ~not_: (function
      | Name f -> some @@ name' @@ Formula.not f
      | Kind f -> some @@ kind' @@ Formula.not f
      | Devisers f -> some @@ devisers' @@ Formula.not f
    )
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Name f1, Name f2) -> some @@ name @@ op f1 f2
      | (Kind f1, Kind f2) -> some @@ kind (op f1 f2)
      | (Devisers f1, Devisers f2) -> some @@ devisers (op f1 f2)
      | _ -> None
    )
    (function
      | Name sfilter -> name @@ Formula_string.optimise sfilter
      | Kind kfilter -> kind @@ Kind.Dance.Filter.optimise kfilter
      | Devisers pfilter -> devisers @@ Formula_list.optimise (Formula_entry.optimise_public Person.optimise) pfilter
    )
