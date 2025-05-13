module type S = sig
  (** {1 Set filter} *)

  type predicate = Core.Set.predicate
  type t = Core.Set.t

  val accepts : t -> ModelBuilder.Core.Set.t Entry.t -> float Lwt.t

  val is : ModelBuilder.Core.Set.t Entry.t -> predicate
  val is' : ModelBuilder.Core.Set.t Entry.t -> t

  val existsVersion : Core.Version.t -> predicate
  val existsVersion' : Core.Version.t -> t

  val existsConceptor : Core.Person.t -> predicate
  val existsConceptor' : Core.Person.t -> t

  val kind : KindDance.Filter.t -> predicate
  val kind' : KindDance.Filter.t -> t

  val memVersion : ModelBuilder.Core.Version.t Entry.t -> predicate
  val memVersion' : ModelBuilder.Core.Version.t Entry.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
