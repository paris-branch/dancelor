module type S = sig
  (** {1 Version filter} *)

  type predicate = Core.Version.predicate
  type t = Core.Version.t

  val accepts : t -> Model_builder.Core.Version.entry -> float Lwt.t

  val is : Model_builder.Core.Version.entry -> predicate
  val is' : Model_builder.Core.Version.entry -> t

  val tune : Core.Tune.t -> predicate
  val tune' : Core.Tune.t -> t

  val key : Music.Key.t -> predicate
  val key' : Music.Key.t -> t

  val sources : Core.Source.t Formula_list.t -> predicate
  val sources' : Core.Source.t Formula_list.t -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
