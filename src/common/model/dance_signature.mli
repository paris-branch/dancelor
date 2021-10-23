open Nes

type t = Dance.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val kind : t -> Kind.dance Lwt.t
val deviser : t -> Credit.t option Lwt.t

(** {2 Filter} *)

module Filter : sig
  include module type of Dance.Filter
  val accepts : t -> Dance.t -> float Lwt.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
