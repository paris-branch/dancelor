open Dancelor_common

(** {1 Tune} *)

type version

val version_content : version -> string

val version_to_jsonm : version -> Ezjsonm.value

type t

val slug : t -> Slug.t
val default_version : t -> version
val version : t -> Slug.t -> version

val to_jsonm : t -> Ezjsonm.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_all :
    ?name:string -> ?author:string ->
    ?kind:Kind.base -> ?keys:Music.key list -> ?mode:Music.mode ->
    unit -> (float * t * version) list
end
