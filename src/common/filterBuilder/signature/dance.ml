module type S = sig
  (** {1 Dance filter} *)

  type predicate = Core.Dance.predicate
  type t = Core.Dance.t
  [@@deriving eq, show]

  val accepts : t -> ModelBuilder.Core.Dance.entry -> float Lwt.t

  val is : ModelBuilder.Core.Dance.entry -> predicate
  val is' : ModelBuilder.Core.Dance.entry -> t

  val kind : Kind.Dance.Filter.t -> predicate
  val kind' : Kind.Dance.Filter.t -> t

  val exists_deviser : Core.Person.t -> predicate
  val exists_deviser' : Core.Person.t -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
