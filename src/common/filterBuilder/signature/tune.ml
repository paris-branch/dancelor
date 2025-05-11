module type S = sig
  (** {1 Tune filter} *)

  type predicate = Core.Tune.predicate
  type t = Core.Tune.t

  val accepts : t -> ModelBuilder.Core.Tune.t Entry.t -> float Lwt.t
  (** The main function for filters: given a filter and a tune, [accepts]
      returns a float between [0.] and [1.] representing how much the filter
      accepts the tune, [1.] meaning that the tune is fully accepted and [0.]
      meaning that the tune is fully rejected. *)

  val is : ModelBuilder.Core.Tune.t Entry.t -> predicate
  val is' : ModelBuilder.Core.Tune.t Entry.t -> t
  (** [is tune] is a filter that matches exactly [tune] and only [tune]. *)

  val kind : KindBase.Filter.t -> predicate
  val kind' : KindBase.Filter.t -> t

  val existsComposer : Core.Person.t -> predicate
  val existsComposer' : Core.Person.t -> t

  val existsComposerIs : ModelBuilder.Core.Person.t Entry.t -> predicate
  val existsComposerIs' : ModelBuilder.Core.Person.t Entry.t -> t

  val existsDance : Core.Dance.t -> predicate
  val existsDance' : Core.Dance.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  (** Converter from text formulas to formulas on tunes. *)

  val from_text_formula : TextFormula.t -> (t, string) Result.t
  (** Build a filter from a text predicate, or fail. *)

  val from_string : ?filename: string -> string -> (t, string) Result.t
  (** Build a filter from a string, or fail. *)

  val to_string : t -> string

  val optimise : t -> t
end
