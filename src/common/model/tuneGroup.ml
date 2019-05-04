open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    kind : Kind.base ;
    author : Credit.t Slug.t option ;
    remark : string }
[@@deriving yojson]

let slug tune_group = tune_group.slug
let name tune_group = tune_group.name
let kind tune_group = tune_group.kind
let author tune_group = tune_group.author
