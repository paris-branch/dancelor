open Nes

type predicate =
  | Name of Formula_string.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name

let converter =
  Text_formula_converter.(
    make
      ~raw: (ok % name' % Formula_string.matches')
      [
        unary_lift ~name: "name" (name, name_val) ~converter: Formula_string.converter;
      ]
  )

let optimise =
  Formula.optimise
    ~up: (fun {is_tf} ->
      function
        | Name f -> is_tf f
    )
    ~not_: (function
      | Name f -> some @@ name @@ Formula.not f
    )
    ~binop: (fun {op} f1 f2 ->
      match (f1, f2) with
      | (Name f1, Name f2) -> some @@ name @@ op f1 f2
    )
    (function
      | Name sfilter -> name @@ Formula_string.optimise sfilter
    )
