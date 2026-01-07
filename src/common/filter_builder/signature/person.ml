module type S = sig
  (** {1 Person filter} *)

  type predicate = Core.Person.predicate
  type t = Core.Person.t

  val accepts : t -> Model_builder.Core.Person.entry -> float Lwt.t

  val is : Model_builder.Core.Person.entry -> predicate
  val is' : Model_builder.Core.Person.entry -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
