open Nes

type predicate =
  | Username of Formula_string.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let username' = Formula.pred % username

let converter =
  Text_formula_converter.(
    make
      ~debug_name: "user"
      ~debug_print: pp_predicate
      ~raw: (ok % username' % Formula_string.matches')
      [
        unary_lift ~name: "username" (username, username_val) ~converter: Formula_string.converter;
      ]
  )

let optimise =
  Formula.optimise
    (function
      | Username sfilter -> username @@ Formula_string.optimise sfilter
    )

let accepts filter user =
  Formula.interpret filter @@ function
    | Username sfilter ->
      Formula_string.accepts sfilter @@ Model_builder.Core.User.Username.to_string @@ Model_builder.Core.User.username user
