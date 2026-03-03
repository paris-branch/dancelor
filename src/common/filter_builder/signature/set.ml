module type S = sig
  (** {1 Set filter} *)

  type predicate = Core.Set.predicate
  type t = Core.Set.t

  val versions : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public Formula_list.t -> predicate
  val versions' : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public Formula_list.t -> t

  val conceptors : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> predicate
  val conceptors' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> t

  val kind : Kind.Dance.Filter.t -> predicate
  val kind' : Kind.Dance.Filter.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Set.t -> float Lwt.t
end
