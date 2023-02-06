open Nes

type t = DanceCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val kind : t -> Kind.dance Lwt.t
val deviser : t -> CreditCore.t option Lwt.t
val two_chords : t -> bool Lwt.t
val scddb_id : t -> int option Lwt.t
val disambiguation : t -> string Lwt.t

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status: Status.t ->
  name: string ->
  kind: Kind.dance ->
  ?deviser: CreditCore.t ->
  two_chords: bool ->
  ?scddb_id: int ->
  unit ->
  t Lwt.t

val search :
  ?pagination: Pagination.t ->
  ?threshold: float ->
  DanceFilter.t ->
  t Score.t list Lwt.t
