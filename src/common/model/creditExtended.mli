include (module type of Credit)

val persons : t -> Person.t list Lwt.t
