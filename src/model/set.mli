open Dancelor_common

type t

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_all : unit -> t list
end
