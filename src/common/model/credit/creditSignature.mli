open Nes

type t = CreditCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val line : t -> string Lwt.t
val persons : t -> PersonCore.t list Lwt.t
val scddb_id : t -> int option Lwt.t

val is_trad : t -> bool

val equal : t -> t -> bool Lwt.t

(** {2 Filters} *)

module Filter : sig
  type t = CreditCore.Filter.t

  val accepts : t -> CreditCore.t -> float Lwt.t

  val is : CreditCore.t -> t
  val memPerson : PersonCore.t -> t

  val raw : string -> t TextFormula.or_error
  val nullary_text_predicates : (string * t) list
  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  line:string ->
  ?persons:PersonCore.t list ->
  ?scddb_id:int ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
