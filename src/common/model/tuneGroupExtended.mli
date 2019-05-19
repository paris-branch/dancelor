include (module type of TuneGroup)

val get : t NesSlug.t -> t Lwt.t

val author : t -> Credit.t option Lwt.t
