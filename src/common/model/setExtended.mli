include (module type of Set)

val get : t NesSlug.t -> t Lwt.t
val get_all : unit -> t list Lwt.t
val save : ?slug:t NesSlug.t -> name:string -> kind:Kind.dance -> ?status:Status.t -> tunes:Tune.t list -> unit -> t Lwt.t
val delete : t -> unit Lwt.t

val deviser : t -> Credit.t option Lwt.t
val tunes : t -> Tune.t list Lwt.t
