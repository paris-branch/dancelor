include (module type of Tune)

val group : t -> TuneGroup.t Lwt.t
val arranger : t -> Credit.t option Lwt.t
val dances : t -> Dance.t list Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t

val all :
  ?filter:TuneFilter.t -> ?pagination:Pagination.t ->
  unit -> t list Lwt.t

val search :
  ?filter:TuneFilter.t -> ?pagination:Pagination.t ->
  ?threshold:float -> string list ->
  t Score.t list Lwt.t
