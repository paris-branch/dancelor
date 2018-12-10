open Dancelor_common

(** {1 Tune} *)

module Version : sig
  type t

  val content : t -> string

  val to_jsonm : t -> Ezjsonm.value
end

type t

val slug : t -> Slug.t
val default_version : t -> Version.t
val version : t -> Slug.t -> Version.t

val to_jsonm : t -> Ezjsonm.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_all :
    ?name:string -> ?author:string ->
    ?kind:Kind.base -> ?keys:Music.key list -> ?mode:Music.mode ->
    unit -> (float * t * Version.t) list
end
