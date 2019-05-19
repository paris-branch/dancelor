include (module type of Credit)

val persons : t -> Person.t list Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t
