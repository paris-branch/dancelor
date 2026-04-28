open Nes

type predicate =
  | Username of Formula_string.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, ord, show {with_path = false}, yojson]

let username' = Formula.pred % username

let converter : predicate Text_formula_converter.t =
  Text_formula_converter.(
    make
      ~debug_name: "user"
      ~debug_print: pp_predicate
      ~raw: (ok % username' % Formula_string.matches')
      ~lifters: [
        lifter ~name: "username" (username, username_val) Formula_string.converter;
      ]
      []
      ~compare_predicate
  )

let accepts filter user =
  Formula.interpret filter @@ function
    | Username sfilter ->
      Formula_string.accepts sfilter @@ Username.to_string @@ Model_builder.Core.User.username user
