open Dancelor_common

type t

val slug : t -> t Slug.t
val name : t -> string
val kind : t -> Kind.dance
val tunes : t -> Tune.t Slug.t list
val deviser : t -> Credit.t Slug.t option
val contains : Tune.t Slug.t -> t -> bool

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

val serialize : t -> Json.t
val unserialize : Json.t -> t
