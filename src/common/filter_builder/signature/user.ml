module type S = sig
  (** {1 User filter} *)

  type predicate = Formula_user.predicate
  type t = Formula_user.t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.User.t -> float Lwt.t
end
