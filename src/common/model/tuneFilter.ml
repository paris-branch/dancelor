open Nes

type t =
  { group_author : Credit.t Slug.t list [@default []] ;
    group_kind : Kind.base list         [@default []] ;
    key : Music.key list                [@default []] ;
    bars : int list                     [@default []] }
[@@deriving yojson, make]

let make ?group_author ?group_kind ?key ?bars () =
  Lwt.return (make ?group_author ?group_kind ?key ?bars ())

let group_author f = Lwt.return f.group_author
let group_kind f = Lwt.return f.group_kind
let key f = Lwt.return f.key
let bars f = Lwt.return f.bars
