open Dancelor_common

(** {1 Credit} *)

type t

val slug : t -> Slug.t
val line : t -> string

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_opt : Slug.t -> t option

  val save :
    ?slug:Slug.t ->
    line:string ->
    ?persons:Person.t list ->
    unit -> t
end
