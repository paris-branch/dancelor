open Nes

let _key = "dance"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.Dance.t ;
    devisers : PersonCore.t Slug.t list [@default []] ;
    two_chords : bool option [@default None] [@key "two-chords"] ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    disambiguation : string [@default ""] ;
    date : PartialDate.t option [@default None] ; (** When the dance was devised. *)
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving make, show {with_path = false}, yojson, fields]

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
  [@@deriving eq, show {with_path = false}, yojson, variants]

  type t = predicate Formula.t
  [@@deriving eq, show {with_path = false}, yojson]

  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let kind' = Formula.pred % kind
  let existsDeviser' = Formula.pred % existsDeviser
end
