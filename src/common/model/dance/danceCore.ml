open Nes

let _key = "dance"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.Dance.t ;
    deviser : PersonCore.t Slug.t option [@default None] ;
    two_chords : bool [@default false] [@key "two-chords"] ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    disambiguation : string [@default ""] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: PPX *)
let slug dance = dance.slug
let status dance = dance.status
let name dance = dance.name
let kind dance = dance.kind
let deviser dance = dance.deviser
let two_chords dance = dance.two_chords
let scddb_id dance = dance.scddb_id
let disambiguation dance = dance.disambiguation
let modified_at dance = dance.modified_at
let created_at dance = dance.created_at

module Filter = struct
  let _key = "dance-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
    | Kind of Kind.Dance.Filter.t
    | Deviser of PersonCore.Filter.t (** deviser is defined and passes the filter *)
  [@@deriving show {with_path = false}, yojson]

  (* FIXME: PPX *)
  let is dance = Is dance
  let name name = Name name
  let nameMatches name = NameMatches name
  let kind kfilter = Kind kfilter
  let deviser cfilter = Deviser cfilter

  let unName = function Name n -> Some n | _ -> None
  let unNameMatches = function NameMatches n -> Some n | _ -> None
  let unKind = function Kind kf -> Some kf | _ -> None
  let unDeviser = function Deviser cf -> Some cf | _ -> None

  type t = predicate Formula.t
  [@@deriving show {with_path = false}, yojson]

  let is' = Formula.pred % is
  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let kind' = Formula.pred % kind
  let deviser' = Formula.pred % deviser
end
