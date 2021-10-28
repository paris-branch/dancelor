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
    disambiguation : string           [@default ""] }
[@@deriving yojson]

let slug t = Lwt.return t.slug
let status t = Lwt.return t.status
let tune t = Lwt.return t.tune
let bars t = Lwt.return t.bars
let key t = Lwt.return t.key
let structure t = Lwt.return t.structure
let arranger t = Lwt.return t.arranger
let remark t = Lwt.return t.remark
let disambiguation t = Lwt.return t.disambiguation

let equal version1 version2 =
  let%lwt slug1 = slug version1 in
  let%lwt slug2 = slug version2 in
  Lwt.return (Slug.equal slug1 slug2)