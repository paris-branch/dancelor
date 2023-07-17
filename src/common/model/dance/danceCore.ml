open Nes

let _key = "dance"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string ;
    kind : Kind.Dance.t ;
    deviser : CreditCore.t Slug.t option [@default None] ;
    two_chords : bool [@default false] [@key "two-chords"] ;
    scddb_id : int option [@default None] [@key "scddb-id"] ;
    disambiguation : string [@default ""] ;
    modified_at : Datetime.t [@key "modified-at"] ;
    created_at  : Datetime.t [@key "created-at"] }
[@@deriving make, yojson]

let slug d = Lwt.return d.slug
let status d = Lwt.return d.status
let deviser d = Lwt.return d.deviser

module Filter = struct
  let _key = "dance-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
    | Kind of KindFilter.Dance.t
    | Deviser of CreditCore.Filter.t (** deviser is defined and passes the filter *)
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
