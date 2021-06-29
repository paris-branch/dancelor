open Nes

type t = Tune.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val alternative_names : t -> string list Lwt.t
val kind : t -> Kind.base Lwt.t
val author : t -> Credit.t option Lwt.t
val dances : t -> Dance.t list Lwt.t
val remark : t -> string Lwt.t

(** {2 Filter} *)

module Filter : sig
  include module type of Tune.Filter
  val accepts : t -> Tune.t -> bool Lwt.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val all :
  ?filter:Filter.t ->
  ?pagination:Pagination.t ->
  unit -> t list Lwt.t

val search :
  ?filter:Filter.t ->
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t
