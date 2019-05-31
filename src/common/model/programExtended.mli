include (module type of Program)

val sets : t -> Set.t list Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t

val get_all : unit -> t list Lwt.t
