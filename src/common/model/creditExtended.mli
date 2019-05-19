include (module type of Credit)

val get : t NesSlug.t -> t Lwt.t

val persons : t -> Person.t list Lwt.t
