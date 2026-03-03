module type S = sig
  (** {1 Book filter} *)

  type predicate = Core.Book.predicate
  type t = Core.Book.t

  val editors : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> predicate
  val editors' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> t

  val sets : Core.Set.t Formula_list.t -> predicate
  val sets' : Core.Set.t Formula_list.t -> t

  val versions : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public Formula_list.t -> predicate
  val versions' : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public Formula_list.t -> t

  val versions_deep : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public Formula_list.t -> predicate
  val versions_deep' : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public Formula_list.t -> t

  val owners : Core.User.t Formula_list.t -> predicate
  val owners' : Core.User.t Formula_list.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Book.entry -> float Lwt.t
end
