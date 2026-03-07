module type S = sig
  (** {1 Person filter} *)

  type predicate = Core.Person.predicate
  type t = Core.Person.t

  val name' : Formula_string.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Person.t -> float Lwt.t
end
