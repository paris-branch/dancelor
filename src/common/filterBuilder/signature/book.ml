module type S = sig
  (** {1 Book filter} *)

  type predicate = Core.Book.predicate
  type t = Core.Book.t

  val accepts : t -> ModelBuilder.Core.Book.t Entry.t -> float Lwt.t

  val existseditor' : Core.Person.t -> t

  val memversion : ModelBuilder.Core.Version.t Entry.t -> predicate
  val memversion' : ModelBuilder.Core.Version.t Entry.t -> t

  val memset : ModelBuilder.Core.Set.t Entry.t -> predicate
  val memset' : ModelBuilder.Core.Set.t Entry.t -> t

  val memtunedeep' : ModelBuilder.Core.Tune.t Entry.t -> t
  (** Matches if the given tune appears in any version at any depth in the book,
      that is directly in the book or in a set of the book. *)

  val memversiondeep' : ModelBuilder.Core.Version.t Entry.t -> t
  (** Matches if the given version appears at any depth in the book, that is
      directly in the book or in a set of the book. *)

  val existstunedeep' : Core.Tune.t -> t
  val existsversiondeep' : Core.Version.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
