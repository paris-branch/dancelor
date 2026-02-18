module type S = sig
  (** {1 Tune filter} *)

  type predicate = Core.Tune.predicate
  type t = Core.Tune.t

  val accepts : t -> Model_builder.Core.Tune.entry -> float Lwt.t
  (** The main function for filters: given a filter and a tune, [accepts]
      returns a float between [0.] and [1.] representing how much the filter
      accepts the tune, [1.] meaning that the tune is fully accepted and [0.]
      meaning that the tune is fully rejected. *)

  val is : Model_builder.Core.Tune.entry -> predicate
  val is' : Model_builder.Core.Tune.entry -> t
  (** [is tune] is a filter that matches exactly [tune] and only [tune]. *)

  val kind : Kind.Base.Filter.t -> predicate
  val kind' : Kind.Base.Filter.t -> t

  val composers : Core.Person.t Formula_list.t -> predicate
  val composers' : Core.Person.t Formula_list.t -> t

  val dances : Core.Dance.t Formula_list.t -> predicate
  val dances' : Core.Dance.t Formula_list.t -> t

  val text_formula_converter : predicate Text_formula_converter.t
  (** Converter from text formulas to formulas on tunes. *)

  val from_text_formula : Text_formula.t -> (t, string) Result.t
  (** Build a filter from a text predicate, or fail. *)

  val from_string : ?filename: string -> string -> (t, string) Result.t
  (** Build a filter from a string, or fail. *)

  val to_string : t -> string

  val optimise : t -> t
end
