module type S = sig
  (** {1 Tune} *)

  open Nes
  open Dancelor_common_database
  open Dancelor_common_model_utils
  open Dancelor_common_model_core

  type t = Tune.t

  val make :
    name: string ->
    ?alternative_names: string list ->
    kind: Kind.Base.t ->
    ?composers: Person.t Entry.t list ->
    ?dances: Dance.t Entry.t list ->
    ?remark: string ->
    ?scddb_id: int ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t Entry.t -> string
  val alternative_names : t Entry.t -> string list
  val kind : t Entry.t -> Kind.Base.t
  val composers : t Entry.t -> Person.t Entry.t list Lwt.t
  val dances : t Entry.t -> Dance.t Entry.t list Lwt.t
  val remark : t Entry.t -> string
  val scddb_id : t Entry.t -> int option
  val date : t Entry.t -> PartialDate.t option

  val equal : t -> t -> bool

  val compare : t Entry.t -> t Entry.t -> int (* FIXME: sounds hackish *)

  (** {2 Filters} *)

  module Filter : sig
    type predicate = [%import: Dancelor_common_model_filter.Tune.predicate]
    type t = [%import: Dancelor_common_model_filter.Tune.t]
    [@@deriving eq, show]

    val accepts : t -> Tune.t Entry.t -> float Lwt.t
    (** The main function for filters: given a filter and a tune, [accepts]
        returns a float between [0.] and [1.] representing how much the filter
        accepts the tune, [1.] meaning that the tune is fully accepted and [0.]
        meaning that the tune is fully rejected. *)

    val is : Tune.t Entry.t -> predicate
    val is' : Tune.t Entry.t -> t
    (** [is tune] is a filter that matches exactly [tune] and only [tune]. *)

    val kind : KindBase.Filter.t -> predicate
    val kind' : KindBase.Filter.t -> t

    val existsComposer : Dancelor_common_model_filter.Person.t -> predicate
    val existsComposer' : Dancelor_common_model_filter.Person.t -> t

    val existsComposerIs : Person.t Entry.t -> predicate
    val existsComposerIs' : Person.t Entry.t -> t

    val existsDance : Dancelor_common_model_filter.Dance.t -> predicate
    val existsDance' : Dancelor_common_model_filter.Dance.t -> t

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

  val get : t Slug.t -> t Entry.t Lwt.t

  val create : t -> t Entry.t Lwt.t
  (** Create a new database entry for the given tune. *)

  val update : t Slug.t -> t -> t Entry.t Lwt.t
  (** Update an existing database entry with the given tune. *)

  val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
  (** Either {!create} or {!update}. *)

  val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
  (** Returns the list of all the tunes that match the filter with a score higher
      than the hardcoded threshold. The first element of the pair is the number of
      tunes. The second element of the pair is a slice of the list, taken as per
      the slice. *)

  val search' : Filter.t -> t Entry.t list Lwt.t
  (** Like {!search} but returns only the list of all values. *)

  val count : Filter.t -> int Lwt.t
  (** Like {!search} but returns only the number of items. *)
end
