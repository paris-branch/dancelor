open Nes

type t = Version.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val tune : t -> Tune.t Lwt.t
val bars : t -> int Lwt.t
val key : t -> Music.key Lwt.t
val structure : t -> string Lwt.t
val arranger : t -> Credit.t option Lwt.t
val sources : t -> Source.t list Lwt.t
val remark : t -> string Lwt.t
val disambiguation : t -> string Lwt.t

val content : t -> string Lwt.t

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val all :
  ?filter:VersionFilter.t -> ?pagination:Pagination.t ->
  unit -> t list Lwt.t

val search :
  ?filter:VersionFilter.t ->
  ?pagination:Pagination.t ->
  ?threshold:float -> string ->
  t Score.t list Lwt.t

val count :
  ?filter:VersionFilter.t ->
  unit -> int Lwt.t
