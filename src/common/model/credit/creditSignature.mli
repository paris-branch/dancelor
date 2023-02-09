open Nes

type t = CreditCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val line : t -> string Lwt.t
val persons : t -> PersonCore.t list Lwt.t
val scddb_id : t -> int option Lwt.t

val is_trad : t -> bool

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status: Status.t ->
  line: string ->
  ?persons: PersonCore.t list ->
  ?scddb_id: int ->
  unit ->
  t Lwt.t

val search :
  ?pagination: Pagination.t ->
  ?threshold: float ->
  CreditFilter.t ->
  t Score.t list Lwt.t
