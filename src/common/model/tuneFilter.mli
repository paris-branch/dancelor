open Nes

type t

val make :
  ?group_author:Credit.t Slug.t list ->
  ?group_kind:Kind.base list -> ?key:Music.key list -> ?bars:int list ->
  unit -> t Lwt.t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val group_author : t -> Credit.t Slug.t list Lwt.t
val group_kind : t -> Kind.base list Lwt.t
val key : t -> Music.key list Lwt.t
val bars : t -> int list Lwt.t
