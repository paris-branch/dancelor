open Nes

type t =
  { tune : Tune.t Slug.t list           [@default []] ;
    tune_author : Credit.t Slug.t list  [@default []] ;
    tune_kind : Kind.base list          [@default []] ;
    key : Music.key list                [@default []] ;
    bars : int list                     [@default []] }
[@@deriving yojson, make]

let _key = "version-filter"

let make ?tune ?tune_author ?tune_kind ?key ?bars () =
  Lwt.return (make ?tune ?tune_author ?tune_kind ?key ?bars ())

let tune f = Lwt.return f.tune
let tune_author f = Lwt.return f.tune_author
let tune_kind f = Lwt.return f.tune_kind
let key f = Lwt.return f.key
let bars f = Lwt.return f.bars

let add_key k t =
  Lwt.return {t with key = k :: t.key}

let remove_key k t =
  Lwt.return {t with key = List.filter ((<>) k) t.key}

let add_bars b t =
  Lwt.return {t with bars = b :: t.bars}

let remove_bars b t =
  Lwt.return {t with bars = List.filter ((<>) b) t.bars}

let add_kind k t =
  Lwt.return {t with tune_kind = k :: t.tune_kind}

let remove_kind k t =
  Lwt.return {t with tune_kind = List.filter ((<>) k) t.tune_kind}

module type S = sig
  type nonrec t = t

  val tune_author : t -> Credit.t list Lwt.t
  val tune_kind : t -> Kind.base list Lwt.t
  val key : t -> Music.key list Lwt.t
  val bars : t -> int list Lwt.t

  val add_key : Music.key -> t -> t Lwt.t
  val remove_key : Music.key -> t -> t Lwt.t
  val add_bars : int -> t -> t Lwt.t
  val remove_bars : int -> t -> t Lwt.t
  val add_kind : Kind.base -> t -> t Lwt.t
  val remove_kind : Kind.base -> t -> t Lwt.t

  val make :
    ?tune:Tune.t list ->
    ?tune_author:Credit.t list -> ?tune_kind:Kind.base list ->
    ?key:Music.key list -> ?bars: int list ->
    unit -> t Lwt.t
end

let make ?tune ?tune_author ?tune_kind ?key ?bars () =
  let%lwt tune =
    match tune with
    | None -> Lwt.return_none
    | Some tune ->
      let%lwt tune = Lwt_list.map_s Tune.slug tune in
      Lwt.return_some tune
  in
  let%lwt tune_author =
    match tune_author with
    | None -> Lwt.return_none
    | Some tune_author ->
      let%lwt tune_author = Lwt_list.map_s Credit.slug tune_author in
      Lwt.return_some tune_author
  in
  make ?tune ?tune_author ?tune_kind ?key ?bars ()
