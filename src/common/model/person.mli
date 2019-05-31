open Nes

type t

val slug : t -> t Slug.t Lwt.t
val name : t -> string Lwt.t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
