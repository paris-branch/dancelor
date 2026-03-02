module type S = sig
  (** {1 Tune filter} *)

  type predicate = Core.Tune.predicate
  type t = Core.Tune.t

  val is : Model_builder.Core.Tune.entry -> predicate
  val is' : Model_builder.Core.Tune.entry -> t
  (** [is tune] is a filter that matches exactly [tune] and only [tune]. *)

  val kind : Kind.Base.Filter.t -> predicate
  val kind' : Kind.Base.Filter.t -> t

  val composers : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.t Formula_list.t -> predicate
  val composers' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.t Formula_list.t -> t

  val dances : Core.Dance.t Formula_list.t -> predicate
  val dances' : Core.Dance.t Formula_list.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Tune.entry -> float Lwt.t
end
