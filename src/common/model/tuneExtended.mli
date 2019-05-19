include (module type of Tune)

val group : t -> TuneGroup.t Lwt.t
val arranger : t -> Credit.t option Lwt.t
