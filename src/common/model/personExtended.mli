include (module type of Person)

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t
