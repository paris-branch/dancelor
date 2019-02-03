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

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

(** {2 Database} *)

module Database : sig

  type db

  val create : unit -> db
  val fill : db -> Json.t list -> unit

  val initialise : unit -> unit

  val get : ?db:db -> Slug.t -> t
  val get_opt : Slug.t -> t option

  val mem : ?db:db -> Slug.t -> bool

  val get_all :
    ?db:db ->
    ?name:string -> ?author:string ->
    ?kind:Kind.base -> ?keys:Music.key list -> ?mode:Music.mode ->
    unit -> (float * t) list
end
