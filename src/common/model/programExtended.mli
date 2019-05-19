include (module type of Program)

val get : t NesSlug.t -> t Lwt.t
val get_all : unit -> t list Lwt.t

val sets : t -> Set.t list Lwt.t
