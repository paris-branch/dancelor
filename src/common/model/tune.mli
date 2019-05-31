open Nes

type t

val slug : t -> t Slug.t Lwt.t
val group : t -> TuneGroup.t Slug.t Lwt.t
val bars : t -> int Lwt.t
val key : t -> Music.key Lwt.t
val structure : t -> string Lwt.t
val arranger : t -> Credit.t Slug.t option Lwt.t
val sources : t -> string list Lwt.t
val dances : t -> Dance.t Slug.t list Lwt.t
val remark : t -> string Lwt.t
val disambiguation : t -> string Lwt.t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
