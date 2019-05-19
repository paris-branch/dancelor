open Nes

type t

val slug : t -> t Slug.t Lwt.t
val content : t -> string Lwt.t
val group : t -> TuneGroup.t Slug.t Lwt.t
val key : t -> Music.key Lwt.t
val bars : t -> int Lwt.t
val structure : t -> string Lwt.t
val arranger : t -> Credit.t Slug.t option Lwt.t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
