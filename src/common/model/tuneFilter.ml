open Nes

type t =
  { group : TuneGroup.t Slug.t list     [@default []] ;
    group_author : Credit.t Slug.t list [@default []] ;
    group_kind : Kind.base list         [@default []] ;
    key : Music.key list                [@default []] ;
    bars : int list                     [@default []] }
[@@deriving yojson, make]

let _key = "tune-filter"

let make ?group ?group_author ?group_kind ?key ?bars () =
  Lwt.return (make ?group ?group_author ?group_kind ?key ?bars ())

let group f = Lwt.return f.group
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

module type S = sig
  type nonrec t = t

  val group_author : t -> Credit.t list Lwt.t
  val group_kind : t -> Kind.base list Lwt.t
  val key : t -> Music.key list Lwt.t
  val bars : t -> int list Lwt.t

  val add_key : Music.key -> t -> t Lwt.t
  val remove_key : Music.key -> t -> t Lwt.t
  val add_bars : int -> t -> t Lwt.t
  val remove_bars : int -> t -> t Lwt.t
  val add_kind : Kind.base -> t -> t Lwt.t
  val remove_kind : Kind.base -> t -> t Lwt.t

  val make :
    ?group:TuneGroup.t list ->
    ?group_author:Credit.t list -> ?group_kind:Kind.base list ->
    ?key:Music.key list -> ?bars: int list ->
    unit -> t Lwt.t
end

let make ?group ?group_author ?group_kind ?key ?bars () =
  let%lwt group =
    match group with
    | None -> Lwt.return_none
    | Some group ->
      let%lwt group = Lwt_list.map_s TuneGroup.slug group in
      Lwt.return_some group
  in
  let%lwt group_author =
    match group_author with
    | None -> Lwt.return_none
    | Some group_author ->
      let%lwt group_author = Lwt_list.map_s Credit.slug group_author in
      Lwt.return_some group_author
  in
  make ?group ?group_author ?group_kind ?key ?bars ()
