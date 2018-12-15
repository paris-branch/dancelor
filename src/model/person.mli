open Dancelor_common

(** {1 Person} *)

type t

val to_jsonm : t -> Json.value

val slug : t -> Slug.t
val name : t -> string

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t

  val create : name:string -> unit -> Slug.t * t
end
