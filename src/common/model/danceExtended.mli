include module type of Dance

val deviser : t -> Credit.t option Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t
