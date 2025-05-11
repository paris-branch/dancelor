module type S = sig
  (** {1 Person filter} *)

  type predicate = Core.Person.predicate
  type t = Core.Person.t

  val accepts : t -> ModelBuilder.Core.Person.t Entry.t -> float Lwt.t

  val is : ModelBuilder.Core.Person.t Entry.t -> predicate
  val is' : ModelBuilder.Core.Person.t Entry.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
