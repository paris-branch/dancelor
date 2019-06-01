include (module type of Credit)

val persons : t -> Person.t list Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t

val make_and_save :
  line:string ->
  ?persons:Person.t list ->
  unit -> t Lwt.t
