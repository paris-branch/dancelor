module type S = sig
  (** {1 Source filter} *)

  type predicate = Core.Source.predicate
  type t = Core.Source.t

  val editors' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.t Formula_list.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Source.t -> float Lwt.t
end
