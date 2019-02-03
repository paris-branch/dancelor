open Dancelor_common

type t

val slug : t -> Slug.t

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

module Database : sig
  val initialise : unit -> unit

  val get : Slug.t -> t
  val get_opt : Slug.t -> t option

  val get_all : unit -> t list

  val create : name:string -> ?deviser:Credit.t -> kind:Kind.dance -> tunes:Tune.t list -> unit -> t
end
