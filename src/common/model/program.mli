open Nes

type t

val slug : t -> t Slug.t Lwt.t
val name : t -> string Lwt.t
val date : t -> Date.t Lwt.t
val status : t -> Status.t Lwt.t
val sets : t -> Set.t Slug.t list Lwt.t

val contains : Set.t Slug.t -> t -> bool
val compare : t -> t -> int

val to_yojson : t -> NesJson.t
val of_yojson : NesJson.t -> (t, string) result
