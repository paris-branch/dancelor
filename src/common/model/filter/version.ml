open Nes
open Dancelor_common_model_utils

type predicate =
  | Is of Dancelor_common_model_core.Version.t Slug.t
  | Tune of Tune.t
  | Key of Music.key
  | Kind of Kind.Version.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let tune' = Formula.pred % tune
let key' = Formula.pred % key
let kind' = Formula.pred % kind
