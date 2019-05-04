open Nes

type t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val slug : t -> t Slug.t
val name : t -> string
