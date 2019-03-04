open Dancelor_common

type t

val slug : t -> Slug.t
val name : t -> string
val kind : t -> Kind.dance
val tunes : t -> Tune.t list
val deviser : t -> Credit.t option
val contains : Tune.t -> t -> bool

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

module Database : sig
  val initialise : unit -> unit

  val get_opt : Slug.t -> t option
  val get_all : unit -> t list

  val save : ?slug:Slug.t -> name:string -> ?deviser:Credit.t ->
    kind:Kind.dance -> ?status:Status.t -> tunes:Tune.t list -> unit -> t

  val delete : t -> unit
end
