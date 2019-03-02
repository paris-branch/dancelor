open Dancelor_common

(** {1 Person} *)

type t

val to_json : t -> Json.t
val to_jsonm : t -> Json.value

val of_json : Json.t -> t
val of_jsonm : Json.value -> t

val slug : t -> Slug.t
val name : t -> string

(** {2 Database} *)

module Database : sig
  val initialise : unit -> unit
  val report_without_accesses : unit -> unit

  val get_opt : Slug.t -> t option

  val save : ?slug:string -> name:string -> unit -> t
end
