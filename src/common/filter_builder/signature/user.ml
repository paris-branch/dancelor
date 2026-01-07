module type S = sig
  (** {1 User filter} *)

  type predicate = Core.User.predicate
  type t = Core.User.t

  val accepts : t -> Model_builder.Core.User.entry -> float Lwt.t

  val is : Model_builder.Core.User.entry -> predicate
  val is' : Model_builder.Core.User.entry -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
