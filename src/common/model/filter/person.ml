open Nes
open Dancelor_common_model_utils

type predicate =
  | Is of Dancelor_common_model_core.Person.t Slug.t
  | Name of string
  | NameMatches of string
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
