open Nes


type predicate =
  | Is of Core.Tune.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsComposer of Person.t (** one of the composers of the list passes the filter *)
  | Kind of Kind.Base.Filter.t
  | ExistsDance of Dance.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let name' = Formula.pred % name
let nameMatches' = Formula.pred % nameMatches
let existsComposer' = Formula.pred % existsComposer
let kind' = Formula.pred % kind
let existsDance' = Formula.pred % existsDance
