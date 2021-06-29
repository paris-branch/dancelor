open Nes

type t = Person.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t

(** {2 Filter} *)

module Filter : sig
  include module type of Person.Filter
  val accepts : t -> Person.t -> bool Lwt.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t
