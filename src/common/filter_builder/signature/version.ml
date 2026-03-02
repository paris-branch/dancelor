module type S = sig
  (** {1 Version filter} *)

  type predicate = Core.Version.predicate
  type t = Core.Version.t

  val tune : (Model_builder.Core.Tune.t, Core.Tune.t) Formula_entry.t -> predicate
  val tune' : (Model_builder.Core.Tune.t, Core.Tune.t) Formula_entry.t -> t

  val key : Music.Key.t -> predicate
  val key' : Music.Key.t -> t

  val sources : (Model_builder.Core.Source.t, Core.Source.t) Formula_entry.t Formula_list.t -> predicate
  val sources' : (Model_builder.Core.Source.t, Core.Source.t) Formula_entry.t Formula_list.t -> t

  val converter : predicate Text_formula_converter.t
  val optimise : t -> t

  val accepts : t -> Model_builder.Core.Version.t -> float Lwt.t
end
