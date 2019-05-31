open Nes

type t

val slug : t -> t Slug.t Lwt.t
val name : t -> string Lwt.t
val deviser : t -> Credit.t Slug.t option Lwt.t
val kind : t -> Kind.dance Lwt.t
val status : t -> Status.t Lwt.t
val tunes : t -> Tune.t Slug.t list Lwt.t

val contains : Tune.t Slug.t -> t -> bool

val to_yojson : t -> Json.t
val of_yojson : Json.t -> (t, string) result

val unsafe_make :
  slug:t Slug.t ->
  name:string ->
  ?deviser:Credit.t ->
  kind:Kind.dance ->
  ?status:Status.t ->
  ?tunes:Tune.t list ->
  unit -> t Lwt.t
