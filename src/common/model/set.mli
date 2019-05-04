open Nes

type t

val slug : t -> t Slug.t
val name : t -> string
val kind : t -> Kind.dance
val tunes : t -> Tune.t Slug.t list
val deviser : t -> Credit.t Slug.t option
val contains : Tune.t Slug.t -> t -> bool

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
