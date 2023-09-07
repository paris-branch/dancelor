open Nes

let _key = "version"

type t =
  { slug : t Slug.t ;
    status : Status.t                 [@default Status.bot] ;
    tune : TuneCore.t Slug.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    sources : string list [@default []] ; (* FIXME: remove from DB *)
    arranger : CreditCore.t Slug.t option [@default None] ;
    remark : string                   [@default ""] ;
    disambiguation : string           [@default ""] ;
    broken : bool                     [@default false] ;
    modified_at : Datetime.t          [@key "modified-at"] ;
    created_at  : Datetime.t          [@key "created-at"] }
[@@deriving make, yojson]

let slug version = Lwt.return version.slug
let status version = Lwt.return version.status
let tune version = Lwt.return version.tune
let arranger version = Lwt.return version.arranger

module Filter = struct
  let _key = "version-filter"

  type predicate =
    | Is of t
    | Tune of TuneCore.Filter.t
    | Key of Music.key
    | Kind of Kind.Version.Filter.t
    | Broken
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
