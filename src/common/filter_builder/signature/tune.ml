module type S = sig
  (** {1 Tune filter} *)

  type predicate = Core.Tune.predicate
  type t = Core.Tune.t

  val kind : Kind.Base.Filter.t -> predicate
  val kind' : Kind.Base.Filter.t -> t

  val composers : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> predicate
  val composers' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> t

  val dances : (Model_builder.Core.Dance.t, Core.Dance.t) Formula_entry.public Formula_list.t -> predicate
  val dances' : (Model_builder.Core.Dance.t, Core.Dance.t) Formula_entry.public Formula_list.t -> t

  val converter : predicate Text_formula_converter.t
  val accepts : t -> Model_builder.Core.Tune.t -> float Lwt.t
end
