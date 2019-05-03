open Nes

(** {1 Credit} *)

type t

val slug : t -> t Slug.t
val line : t -> string

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

val serialize : t -> Json.t
val unserialize : Json.t -> t
