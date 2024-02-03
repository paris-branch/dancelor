open Nes

let _key = "tune"

type t =
  { slug : t Slug.t ;
    status : Status.t                   [@default Status.bot] ;
    name : string ;
    alternative_names : string list     [@key "alternative-names"] [@default []] ;
    kind : Kind.Base.t ;
    author : PersonCore.t Slug.t option [@default None] ;
    dances : DanceCore.t Slug.t list    [@default []] ;
    remark : string                     [@default ""] ;
    scddb_id : int option               [@default None] [@key "scddb-id"] ;
    modified_at : Datetime.t            [@key "modified-at"] ;
    created_at  : Datetime.t            [@key "created-at"] }
[@@deriving make, yojson]

(* FIXME: PPX *)
let slug tune = tune.slug
let status tune = tune.status
let name tune = tune.name
let alternative_names tune = tune.alternative_names
let kind tune = tune.kind
let author tune = tune.author
let dances tune = tune.dances
let remark tune = tune.remark
let scddb_id tune = tune.scddb_id
let modified_at tune = tune.modified_at
let created_at tune = tune.created_at

let compare = Slug.compare_slugs_or ~fallback:Stdlib.compare slug
let equal tune1 tune2 = compare tune1 tune2 = 0

module Filter = struct
  let _key = "tune-filter"

  type predicate =
    | Is of t
    | Name of string
    | NameMatches of string
    | Author of PersonCore.Filter.t (** author is defined and passes the filter *)
    | Kind of Kind.Base.Filter.t
    | ExistsDance of DanceCore.Filter.t
  [@@deriving yojson]

  (* FIXME: PPX *)
  let is tune = Is tune
  let name string = Name string
  let nameMatches string = NameMatches string
  let author cfilter = Author cfilter
  let kind kfilter = Kind kfilter
  let existsDance dfilter = ExistsDance dfilter

  let authorIs = author % PersonCore.Filter.is'

  type t = predicate Formula.t
  [@@deriving yojson]

  let is' = Formula.pred % is
  let name' = Formula.pred % name
  let nameMatches' = Formula.pred % nameMatches
  let author' = Formula.pred % author
  let kind' = Formula.pred % kind
  let existsDance' = Formula.pred % existsDance

  let authorIs' = Formula.pred % authorIs
end
