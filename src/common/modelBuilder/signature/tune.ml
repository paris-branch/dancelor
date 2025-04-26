module type S = sig
  (** {1 Tune} *)

  open Nes
  open Core

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

  val compare : t Entry.t -> t Entry.t -> int
  (* FIXME: sounds hackish *)

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Tune.predicate
    type t = Filter.Tune.t

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

    val existsComposer : Filter.Person.t -> predicate
    val existsComposer' : Filter.Person.t -> t

    val existsComposerIs : Person.t Entry.t -> predicate
    val existsComposerIs' : Person.t Entry.t -> t

    val existsDance : Filter.Dance.t -> predicate
    val existsDance' : Filter.Dance.t -> t

    val text_formula_converter : predicate TextFormulaConverter.t
    (** Converter from text formulas to formulas on tunes. *)

    val from_text_formula : TextFormula.t -> (t, string) Result.t
    (** Build a filter from a text predicate, or fail. *)

    val from_string : ?filename: string -> string -> (t, string) Result.t
    (** Build a filter from a string, or fail. *)

    val to_string : t -> string

    val optimise : t -> t
  end

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Slug.t -> t Entry.t Lwt.t
end
