open Nes

type t = VersionCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val tune : t -> TuneCore.t Lwt.t
val bars : t -> int Lwt.t
val key : t -> Music.key Lwt.t
val structure : t -> string Lwt.t
val arranger : t -> CreditCore.t option Lwt.t
val remark : t -> string Lwt.t
val disambiguation : t -> string Lwt.t
val broken : t -> bool Lwt.t

val set_broken : t -> bool -> t Lwt.t

val content : t -> string Lwt.t

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  VersionFilter.t ->
  t Score.t list Lwt.t

val count :
  ?threshold:float ->
  VersionFilter.t ->
  int Lwt.t

val mark_broken : t -> unit Lwt.t

val mark_fixed : t -> unit Lwt.t
