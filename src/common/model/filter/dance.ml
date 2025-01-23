open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

type predicate =
  | Is of Dancelor_common_model_core.Dance.t Slug.t
  | Name of string
  | NameMatches of string
  | Kind of Kind.Dance.Filter.t
  | ExistsDeviser of Person.t (** deviser is defined and passes the filter *)
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let kind' = Formula.pred % kind
let existsDeviser' = Formula.pred % existsDeviser
