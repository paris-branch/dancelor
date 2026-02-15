module type S = sig
  (** {1 Book filter} *)

  type predicate = Core.Book.predicate
  type t = Core.Book.t

  val accepts : t -> Model_builder.Core.Book.entry -> float Lwt.t

  val editors' : Core.Person.t Formula_list.t -> t

  val sets : Core.Set.t Formula_list.t -> predicate
  val sets' : Core.Set.t Formula_list.t -> t

  val versions : Core.Version.t Formula_list.t -> predicate
  val versions' : Core.Version.t Formula_list.t -> t

  val versions_deep : Core.Version.t Formula_list.t -> predicate
  val versions_deep' : Core.Version.t Formula_list.t -> t

  val text_formula_converter : predicate Text_formula_converter.t
  val from_text_formula : Text_formula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end
