include (module type of TuneFilter)

val make :
  ?group_author:Credit.t list ->
  ?group_kind:Kind.base list -> ?key:Music.key list -> ?bars: int list ->
  unit -> t Lwt.t

val group_author : t -> Credit.t list Lwt.t
