module type S = sig
  (** {1 Person filter} *)

  type predicate = Core.Person.predicate
  type t = Core.Person.t

  val name' : Formula_string.t -> t

  val accepts : t -> Model_builder.Core.Person.t -> float Lwt.t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
