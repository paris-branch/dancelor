(** {1 Tune} *)

open Nes

type t = TuneCore.t

(** {2 Field getters} *)

val name : t -> string
val alternative_names : t -> string list
val kind : t -> Kind.Base.t
val composers : t -> PersonCore.t list Lwt.t
val dances : t -> DanceCore.t list Lwt.t
val remark : t -> string
val scddb_id : t -> int option
val date : t -> PartialDate.t option

val compare : t -> t -> int
val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: TuneCore.Filter.predicate]
  type t = [%import: TuneCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> TuneCore.t -> float Lwt.t
  (** The main function for filters: given a filter and a tune, [accepts]
      returns a float between [0.] and [1.] representing how much the filter
      accepts the tune, [1.] meaning that the tune is fully accepted and [0.]
      meaning that the tune is fully rejected. *)

  val is : TuneCore.t -> predicate
  val is' : TuneCore.t -> t
  (** [is tune] is a filter that matches exactly [tune] and only [tune]. *)

  val kind : KindBase.Filter.t -> predicate
  val kind' : KindBase.Filter.t -> t

  val existsComposer : PersonCore.Filter.t -> predicate
  val existsComposer' : PersonCore.Filter.t -> t

  val existsComposerIs : PersonCore.t -> predicate
  val existsComposerIs' : PersonCore.t -> t

  val existsDance : DanceCore.Filter.t -> predicate
  val existsDance' : DanceCore.Filter.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  (** Converter from text formulas to formulas on tunes. *)

  val from_text_formula : TextFormula.t -> (t, string) Result.t
  (** Build a filter from a text predicate, or fail. *)

  val from_string : ?filename: string -> string -> (t, string) Result.t
  (** Build a filter from a string, or fail. *)

  val to_string : t -> string

  val optimise : t -> t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val save :
  ?status: Dancelor_common_database.Status.t ->
  name: string ->
  ?alternative_names: string list ->
  kind: Kind.Base.t ->
  ?composers: PersonCore.t list ->
  ?dances: DanceCore.t list ->
  ?remark: string ->
  ?scddb_id: int ->
  ?date: PartialDate.t ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Lwt.t

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the tunes
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of tunes. The second element of the pair
    is a slice of the list, taken as per the [slice] (if any). *)

val search' :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  t list Lwt.t
(** Like {!search} but returns only the list. *)

val count :
  ?threshold: float ->
  Filter.t ->
  int Lwt.t
(** Like {!search} but returns only the number of items. *)
