open Nes

type t =
  { slug : t Slug.t ;
    group : TuneGroup.t Slug.t ;
    bars : int ;
    key : Music.key ;
    structure : string ;
    arranger : Credit.t Slug.t option ;
    content : string }
[@@deriving yojson]

let slug tune = tune.slug
let group tune = tune.group
let key tune = tune.key
let content tune = tune.content
let bars tune = tune.bars
let structure tune = tune.structure
