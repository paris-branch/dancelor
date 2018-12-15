open Dancelor_common

(** {1 Credit} *)

type t

val slug : t -> Slug.t
val line : t -> string

val to_jsonm : t -> Json.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t

  val create :
    line:string ->
    ?persons:Person.t list ->
    unit -> Slug.t * t
end
