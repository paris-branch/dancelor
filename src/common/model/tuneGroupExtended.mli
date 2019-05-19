include (module type of TuneGroup)

val author : t -> Credit.t option Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t
