open Nes

type t =
  { group_author : Credit.t Slug.t list [@default []] ;
    group_kind : Kind.base list         [@default []] ;
    key : Music.key list                [@default []] ;
    bars : int list                     [@default []] }
[@@deriving yojson, make]

let make ?group_author ?group_kind ?key ?bars () =
  Lwt.return (make ?group_author ?group_kind ?key ?bars ())
