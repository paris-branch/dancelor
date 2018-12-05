open Dancelor_common

(** {1 Tune} *)

type t

type view =
  { slug : Slug.t ;
    name : string ;
    author : Credit.view ;
    kind : Kind.tune ;
    (* content : string *) }

val view : t -> view

val view_of_jsonm : Ezjsonm.value -> view
val view_to_jsonm : view -> Ezjsonm.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_all : unit -> t list
end
