open Nes

(** {1 Tune} *)

type t

val slug : t -> t Slug.t
val content : t -> string
val group : t -> TuneGroup.t Slug.t
val key : t -> Music.key
val bars : t -> int
val structure : t -> string

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
