module type S = sig
  (** {1 Book filter} *)

  type predicate = Core.Book.predicate
  type t = Core.Book.t

  val accepts : t -> ModelBuilder.Core.Book.entry -> float Lwt.t

  val exists_editor' : Core.Person.t -> t

  val memversion : ModelBuilder.Core.Version.entry -> predicate
  val memversion' : ModelBuilder.Core.Version.entry -> t

  val memset : ModelBuilder.Core.Set.entry -> predicate
  val memset' : ModelBuilder.Core.Set.entry -> t

  val memtunedeep' : ModelBuilder.Core.Tune.entry -> t
  (** Matches if the given tune appears in any version at any depth in the book,
      that is directly in the book or in a set of the book. *)

  val memversiondeep' : ModelBuilder.Core.Version.entry -> t
  (** Matches if the given version appears at any depth in the book, that is
      directly in the book or in a set of the book. *)

  val existstunedeep' : Core.Tune.t -> t
  val exists_version_deep' : Core.Version.t -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
