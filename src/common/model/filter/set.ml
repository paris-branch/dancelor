open Nes


type predicate =
  | Is of Core.Set.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsConceptor of Person.t (** conceptor is defined and passes the filter *)
  | ExistsVersion of Version.t
  | Kind of Kind.Dance.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let existsConceptor' = Formula.pred % existsConceptor
let existsVersion' = Formula.pred % existsVersion
let kind' = Formula.pred % kind
