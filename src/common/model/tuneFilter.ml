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

let add_key k t =
  Lwt.return {t with key = k :: t.key}

let remove_key k t = 
  Lwt.return {t with key = List.filter (fun k' -> k <> k') t.key}

let add_bars b t = 
  Lwt.return {t with bars = b :: t.bars}

let remove_bars b t = 
  Lwt.return {t with bars = List.filter (fun b' -> b <> b') t.bars}

let add_kind k t = 
  Lwt.return {t with group_kind = k :: t.group_kind}

let remove_kind k t = 
  Lwt.return {t with group_kind = List.filter (fun k' -> k <> k') t.group_kind}
