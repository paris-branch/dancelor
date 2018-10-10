open Dancelor_common

(** {1 Person} *)

type t

type view =
  { slug : Slug.t ;
    name : string }

val view : t -> view

val view_of_jsonm : Ezjsonm.value -> view
val view_to_jsonm : view -> Ezjsonm.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
end
