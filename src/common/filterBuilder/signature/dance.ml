module type S = sig
  (** {1 Dance filter} *)

  type predicate = Core.Dance.predicate
  type t = Core.Dance.t
  [@@deriving eq, show]

  val accepts : t -> ModelBuilder.Core.Dance.t Entry.t -> float Lwt.t

  val is : ModelBuilder.Core.Dance.t Entry.t -> predicate
  val is' : ModelBuilder.Core.Dance.t Entry.t -> t

  val kind : KindDance.Filter.t -> predicate
  val kind' : KindDance.Filter.t -> t

  val existsDeviser : Core.Person.t -> predicate
  val existsDeviser' : Core.Person.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
