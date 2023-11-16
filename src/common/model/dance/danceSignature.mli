(** {1 Dance} *)

open Nes

type t = DanceCore.t

(** {2 Field getters} *)

val slug : t -> t Slug.t
val status : t -> Status.t
val name : t -> string
val kind : t -> Kind.Dance.t
val deviser : t -> PersonCore.t option Lwt.t
val two_chords : t -> bool
val scddb_id : t -> int option
val disambiguation : t -> string
val modified_at : t -> Datetime.t
val created_at  : t -> Datetime.t

val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type t = DanceCore.Filter.t

  val accepts : t -> DanceCore.t -> float Lwt.t

  val is : DanceCore.t -> t
  val deviser : PersonCore.Filter.t -> t

  val raw : string -> t TextFormula.or_error
  val nullary_text_predicates : (string * t) list
  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
  val from_string : ?filename:string -> string -> t TextFormula.or_error
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  kind:Kind.Dance.t ->
  ?deviser:PersonCore.t ->
  two_chords:bool ->
  ?scddb_id:int ->
  ?disambiguation:string ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
