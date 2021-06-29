open Nes

type t =
  { slug : t Slug.t ;
    status : Status.t                 [@default Status.bot] ;
    tune : Tune.t Slug.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t Slug.t option [@default None] ;
    sources : Source.t Slug.t list    [@default []] ;
    remark : string                   [@default ""] ;
    disambiguation : string           [@default ""] }
[@@deriving yojson]

let _key = "version"

let slug t = Lwt.return t.slug
let status t = Lwt.return t.status
let tune t = Lwt.return t.tune
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let sources t = Lwt.return t.sources
let remark t = Lwt.return t.remark
let disambiguation t = Lwt.return t.disambiguation

module Filter = struct
  type version = t
  [@@deriving yojson]

  type t =
    | Is of version
    | Tune of Tune.Filter.t
    | Key of Music.key
    | Bars of int
  [@@deriving yojson]

  let _key = "version-filter"
end
