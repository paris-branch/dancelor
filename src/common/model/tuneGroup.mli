open Nes

(** {1 Tune} *)

type t

val slug : t -> t Slug.t
val name : t -> string
val kind : t -> Kind.base
val author : t -> Credit.t Slug.t option

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
