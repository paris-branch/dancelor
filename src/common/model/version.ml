open Nes

let _key = "version"

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

let equal version1 version2 =
  let%lwt slug1 = slug version1 in
  let%lwt slug2 = slug version2 in
  Lwt.return (Slug.equal slug1 slug2)

module Filter = struct
  let _key = "version-filter"

  type predicate =
    | Is of t
    | Tune of Tune.Filter.t
    | Key of Music.key
    | Bars of int
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is version = Formula.pred (Is version)
  let tune tfilter = Formula.pred (Tune tfilter)
  let tuneIs tune_ = tune (Tune.Filter.is tune_)
  let key key_ = Formula.pred (Key key_)
  let bars bars_ = Formula.pred (Bars bars_)
end
