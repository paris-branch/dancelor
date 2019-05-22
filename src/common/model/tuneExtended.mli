include (module type of Tune)

val group : t -> TuneGroup.t Lwt.t
val arranger : t -> Credit.t option Lwt.t

(** {2 Getters and setters} *)

val get : t NesSlug.t -> t Lwt.t

val get_all :
  ?search:string list ->
  ?threshold:float ->
  ?hard_limit:int ->
  unit -> t Score.t list Lwt.t
