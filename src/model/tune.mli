open Dancelor_common

(** {1 Tune} *)

type t

val slug : t -> Slug.t
val content : t -> string
val group : t -> TuneGroup.t
val key : t -> Music.key
val bars : t -> int
val structure : t -> string

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_opt : Slug.t -> t option

  val get_all : unit -> t list
end
