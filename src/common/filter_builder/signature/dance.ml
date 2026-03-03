module type S = sig
  (** {1 Dance filter} *)

  type predicate = Core.Dance.predicate
  type t = Core.Dance.t
  [@@deriving eq, show]

  val kind : Kind.Dance.Filter.t -> predicate
  val kind' : Kind.Dance.Filter.t -> t

  val devisers : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> predicate
  val devisers' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public Formula_list.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Dance.t -> float Lwt.t
end
