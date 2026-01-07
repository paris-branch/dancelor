module type S = sig
  (** {1 Set filter} *)

  type predicate = Core.Set.predicate
  type t = Core.Set.t

  val accepts : t -> Model_builder.Core.Set.entry -> float Lwt.t

  val is : Model_builder.Core.Set.entry -> predicate
  val is' : Model_builder.Core.Set.entry -> t

  val exists_version : Core.Version.t -> predicate
  val exists_version' : Core.Version.t -> t

  val exists_conceptor : Core.Person.t -> predicate
  val exists_conceptor' : Core.Person.t -> t

  val kind : Kind.Dance.Filter.t -> predicate
  val kind' : Kind.Dance.Filter.t -> t

  val memversion : Model_builder.Core.Version.entry -> predicate
  val memversion' : Model_builder.Core.Version.entry -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
