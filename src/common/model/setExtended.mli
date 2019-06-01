include (module type of Set)

val deviser : t -> Credit.t option Lwt.t
val tunes : t -> Tune.t list Lwt.t

val warnings : t -> warning list Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t

val get_all : unit -> t list Lwt.t

val make_and_save :
  name:string ->
  ?deviser:Credit.t ->
  kind:Kind.dance ->
  ?status:Status.t ->
  ?tunes:Tune.t list ->
  unit -> t Lwt.t

val delete : t -> unit Lwt.t
