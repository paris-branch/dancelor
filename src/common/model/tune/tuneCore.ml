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
    scddb_id : int option               [@default None] [@key "scddb-id"] }
[@@deriving yojson]

let slug tune = Lwt.return tune.slug
let status tune = Lwt.return tune.status
let name tune = Lwt.return tune.name
let alternative_names tune = Lwt.return tune.alternative_names
let kind tune = Lwt.return tune.kind
let author tune = Lwt.return tune.author
let dances tune = Lwt.return tune.dances
let remark tune = Lwt.return tune.remark

let compare =
  compare_slugs_or
    ~fallback:(fun tune1 tune2 ->
        Lwt.return (Stdlib.compare tune1 tune2))
    slug

let equal = equal_from_compare compare
