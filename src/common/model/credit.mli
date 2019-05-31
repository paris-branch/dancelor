open Nes

type t

val slug : t -> t Slug.t Lwt.t
val line : t -> string Lwt.t
val persons : t -> Person.t Slug.t list Lwt.t

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result
