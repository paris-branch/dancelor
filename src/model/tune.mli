open Dancelor_common

(** {1 Tune} *)

type t

val slug : t -> Slug.t
val content : t -> string
val group : t -> TuneGroup.t
val bars : t -> int
val structure : t -> string

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t

  val get_all :
    ?name:string -> ?author:string ->
    ?kind:Kind.base -> ?keys:Music.key list -> ?mode:Music.mode ->
    unit -> (float * t) list
end
