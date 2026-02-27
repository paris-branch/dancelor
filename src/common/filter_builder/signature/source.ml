module type S = sig
  (** {1 Source filter} *)

  type predicate = Core.Source.predicate
  type t = Core.Source.t

  val accepts : t -> Model_builder.Core.Source.entry -> float Lwt.t

  val editors' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.t Formula_list.t -> t

  val is : Model_builder.Core.Source.entry -> predicate
  val is' : Model_builder.Core.Source.entry -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
