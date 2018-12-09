open Dancelor_common

(** {1 Tune} *)

type t

val slug : t -> Slug.t
val content : t -> string

type view =
  { slug : Slug.t ;
    name : string ;
    disambiguation : string ;
    author : Credit.view ;
    kind : Kind.tune ;
    key : Music.key ;
    content : string }

val view : t -> view

val view_of_jsonm : Ezjsonm.value -> view
val view_to_jsonm : view -> Ezjsonm.value

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_all :
    ?name:string -> ?author:string ->
    ?kind:Kind.base -> ?keys:Music.key list -> ?mode:Music.mode ->
    unit -> (float * t) list

  val create :
    name:string ->
    ?disambiguation:string ->
    kind:Kind.tune ->
    key:Music.key ->
    author:Credit.t ->
    content:string ->
    unit -> Slug.t * t
end
