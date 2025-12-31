module type S = sig
  (** {1 Version filter} *)

  type predicate = Core.Version.predicate
  type t = Core.Version.t

  val accepts : t -> ModelBuilder.Core.Version.entry -> float Lwt.t

  val is : ModelBuilder.Core.Version.entry -> predicate
  val is' : ModelBuilder.Core.Version.entry -> t

  val tuneis : ModelBuilder.Core.Tune.entry -> predicate
  val tuneis' : ModelBuilder.Core.Tune.entry -> t

  val tune : Core.Tune.t -> predicate
  val tune' : Core.Tune.t -> t

  val key : Music.Key.t -> predicate
  val key' : Music.Key.t -> t

  val existssource : Core.Source.t -> predicate
  val existssource' : Core.Source.t -> t

  val memsource : ModelBuilder.Core.Source.entry -> predicate
  val memsource' : ModelBuilder.Core.Source.entry -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
