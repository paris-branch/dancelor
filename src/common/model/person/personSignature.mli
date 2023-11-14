open Nes

type t = PersonCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val line : t -> string Lwt.t
val scddb_id : t -> int option Lwt.t

val is_trad : t -> bool

val equal : t -> t -> bool Lwt.t

(** {2 Filters} *)

module Filter : sig
  type t = PersonCore.Filter.t

  val accepts : t -> PersonCore.t -> float Lwt.t

  val is : PersonCore.t -> t

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
  line:string ->
  ?scddb_id:int ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
