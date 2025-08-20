module type S = sig
  (** {1 Version filter} *)

  type predicate = Core.Version.predicate
  type t = Core.Version.t

  val accepts : t -> ModelBuilder.Core.Version.t Entry.t -> float Lwt.t

  val is : ModelBuilder.Core.Version.t Entry.t -> predicate
  val is' : ModelBuilder.Core.Version.t Entry.t -> t

  val tuneis : ModelBuilder.Core.Tune.t Entry.t -> predicate
  val tuneis' : ModelBuilder.Core.Tune.t Entry.t -> t

  val tune : Core.Tune.t -> predicate
  val tune' : Core.Tune.t -> t

  val kind : Kind.Version.Filter.t -> predicate
  val kind' : Kind.Version.Filter.t -> t

  val key : Music.Key.t -> predicate
  val key' : Music.Key.t -> t

  val existssource : Core.Source.t -> predicate
  val existssource' : Core.Source.t -> t

  val memsource : ModelBuilder.Core.Source.t Entry.t -> predicate
  val memsource' : ModelBuilder.Core.Source.t Entry.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
