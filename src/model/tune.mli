open Dancelor_common

(** {1 Tune} *)

type t

val slug : t -> t Slug.t
val content : t -> string
val group : t -> TuneGroup.t Slug.t
val key : t -> Music.key
val bars : t -> int
val structure : t -> string

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

val serialize : t -> Json.t
val unserialize : Json.t -> t
