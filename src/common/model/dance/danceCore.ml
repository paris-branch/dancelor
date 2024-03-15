open Nes

let _key = "dance"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.Dance.t ;
    devisers : PersonCore.t Slug.t list [@default []] ;
    two_chords : bool [@default false] [@key "two-chords"] ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    disambiguation : string [@default ""] ;
    date : PartialDate.t option [@default None] ; (** When the dance was devised. *)
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson]

(* FIXME: PPX *)
let slug dance = dance.slug
let status dance = dance.status
let name dance = dance.name
let kind dance = dance.kind
let devisers dance = dance.devisers
let two_chords dance = dance.two_chords
let scddb_id dance = dance.scddb_id
let disambiguation dance = dance.disambiguation
let date dance = dance.date
let modified_at dance = dance.modified_at
let created_at dance = dance.created_at

module Filter = struct
  let _key = "dance-filter"

  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | Name of string
    | NameMatches of string
    | Kind of Kind.Dance.Filter.t
    | ExistsDeviser of PersonCore.Filter.t (** deviser is defined and passes the filter *)
  [@@deriving eq, show {with_path = false}, yojson]

  (* FIXME: PPX *)
  let is dance = Is dance
  let name name = Name name
  let nameMatches name = NameMatches name
  let kind kfilter = Kind kfilter
  let existsDeviser pfilter = ExistsDeviser pfilter

  let unIs = function Is s -> Some s | _ -> None
  let unName = function Name n -> Some n | _ -> None
  let unNameMatches = function NameMatches n -> Some n | _ -> None
  let unKind = function Kind kf -> Some kf | _ -> None
  let unExistsDeviser = function ExistsDeviser pf -> Some pf | _ -> None

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let kind' = Formula.pred % kind
  let existsDeviser' = Formula.pred % existsDeviser
end
