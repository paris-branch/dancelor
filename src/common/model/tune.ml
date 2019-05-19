open Nes

type t =
  { slug : t Slug.t ;
    group : TuneGroup.t Slug.t        [@key "tune-group"];
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t Slug.t option [@default None] ;
    content : string ;
    sources : string list             [@default []] ; (* FIXME: not string *)
    remark : string                   [@default ""] ;
    disambiguation : string           [@default ""] }
[@@deriving yojson]

let slug t = Lwt.return t.slug
let group t = Lwt.return t.group
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let content t = Lwt.return t.content
