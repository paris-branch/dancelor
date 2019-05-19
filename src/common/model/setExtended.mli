include (module type of Set)

val deviser : t -> Credit.t option Lwt.t
val tunes : t -> Tune.t list Lwt.t
