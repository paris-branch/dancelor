open Nes

type t = TuneCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val alternative_names : t -> string list Lwt.t
val kind : t -> Kind.Base.t Lwt.t
val author : t -> CreditCore.t option Lwt.t
val dances : t -> DanceCore.t list Lwt.t
val remark : t -> string Lwt.t
val scddb_id : t -> int option Lwt.t

val compare : t -> t -> int Lwt.t
val equal : t -> t -> bool Lwt.t

(** {2 Filters} *)

module Filter : sig
  type t = TuneCore.Filter.t

  val accepts : t -> TuneCore.t -> float Lwt.t
  (** The main function for filters: given a filter and a tune, [accepts]
      returns a float between [0.] and [1.] representing how much the filter
      accepts the tune, [1.] meaning that the tune is fully accepted and [0.]
      meaning that the tune is fully rejected. *)

  val is : TuneCore.t -> t
  (** [is tune] is a filter that matches exactly [tune] and only [tune]. *)

  val author : CreditCore.Filter.t -> t
  val authorIs : CreditCore.t -> t
  val existsDance : DanceCore.Filter.t -> t

  val raw : string -> t TextFormula.or_error
  (** Build a filter appropriate to match raw strings, or fail. *)

  val nullary_text_predicates : (string * t) list
  (** Association list of nullary text predicates over sets. *)

  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list
  (** Association list of unary text predicates over tunes. *)

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
  (** Build a filter from a text formula, or fail. *)

  val from_string : ?filename:string -> string -> t TextFormula.or_error
  (** Build a fliter from a string, or fail. *)
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  ?alternative_names:string list ->
  kind:Kind.Base.t ->
  ?author:CreditCore.t ->
  ?dances:DanceCore.t list ->
  ?remark:string ->
  ?scddb_id:int ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
