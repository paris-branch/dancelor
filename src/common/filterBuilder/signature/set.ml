module type S = sig
  (** {1 Set filter} *)

  type predicate = Core.Set.predicate
  type t = Core.Set.t

  val accepts : t -> ModelBuilder.Core.Set.entry -> float Lwt.t

  val is : ModelBuilder.Core.Set.entry -> predicate
  val is' : ModelBuilder.Core.Set.entry -> t

  val existsversion : Core.Version.t -> predicate
  val existsversion' : Core.Version.t -> t

  val existsconceptor : Core.Person.t -> predicate
  val existsconceptor' : Core.Person.t -> t

  val kind : Kind.Dance.Filter.t -> predicate
  val kind' : Kind.Dance.Filter.t -> t

  val memversion : ModelBuilder.Core.Version.entry -> predicate
  val memversion' : ModelBuilder.Core.Version.entry -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
