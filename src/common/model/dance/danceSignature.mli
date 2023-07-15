open Nes

type t = DanceCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val kind : t -> Kind.dance Lwt.t
val deviser : t -> CreditCore.t option Lwt.t
val two_chords : t -> bool Lwt.t
val scddb_id : t -> int option Lwt.t
val disambiguation : t -> string Lwt.t

val equal : t -> t -> bool Lwt.t

(** {2 Filters} *)

module Filter : sig
  type t = DanceCore.Filter.t

  val raw : string -> t TextFormula.or_error

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  kind:Kind.dance ->
  ?deviser:CreditCore.t ->
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
