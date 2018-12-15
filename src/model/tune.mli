open Dancelor_common

(** {1 Tune} *)

type t
type version
type tune_version = t * version

val slug : t -> Slug.t
val default_version : t -> version
val version : t -> Slug.t -> version

val version_content : version -> string

val to_jsonm : t -> Ezjsonm.value
val tune_version_to_jsonms : tune_version -> Ezjsonm.value * Ezjsonm.value
val tune_version_to_jsonm : tune_version -> Ezjsonm.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_all :
    ?name:string -> ?author:string ->
    ?kind:Kind.base -> ?keys:Music.key list -> ?mode:Music.mode ->
    unit -> (float * t * version) list
end
