module type S = sig
  (** {1 User filter} *)

  type predicate = Core.User.predicate
  type t = Core.User.t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.User.t -> float Lwt.t
end
