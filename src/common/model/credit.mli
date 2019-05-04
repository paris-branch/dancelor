open Nes

(** {1 Credit} *)

type t

val slug : t -> t Slug.t
val line : t -> string

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
