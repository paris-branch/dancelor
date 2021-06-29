open Nes

type t = Credit.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val line : t -> string Lwt.t
val persons : t -> Person.t list Lwt.t

val is_trad : t -> bool

(** {2 Filter} *)

module Filter : sig
  type t = Credit.Filter.t =
    | Is of Credit.t
    | ExistsPerson of Person.Filter.t

  val accepts : t -> Credit.t -> bool Lwt.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val all :
  ?filter:Filter.t -> ?pagination:Pagination.t ->
  unit -> t list Lwt.t

val make_and_save :
  ?status:Status.t ->
  line:string ->
  ?persons:Person.t list ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t
