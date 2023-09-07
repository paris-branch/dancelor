open Nes

let _key = "tune"

type t =
  { slug : t Slug.t ;
    status : Status.t                   [@default Status.bot] ;
    name : string ;
    alternative_names : string list     [@key "alternative-names"] [@default []] ;
    kind : Kind.base ;
    author : CreditCore.t Slug.t option [@default None] ;
    dances : DanceCore.t Slug.t list    [@default []] ;
    remark : string                     [@default ""] ;
    scddb_id : int option               [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t            [@key "modified-at"] ;
    created_at  : Datetime.t            [@key "created-at"] }
[@@deriving make, yojson]

let slug tune = Lwt.return tune.slug
let status tune = Lwt.return tune.status
let dances tune = Lwt.return tune.dances
let author tune = Lwt.return tune.author

module Filter = struct
  let _key = "tune-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
    | Author of CreditCore.Filter.t (** author is defined and passes the filter *)
    | Kind of KindFilter.Base.t
    | ExistsDance of DanceCore.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
