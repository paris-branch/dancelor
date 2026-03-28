module type S = sig
  (** {1 Any filter} *)

  type predicate = Core.Any.predicate =
    | Source of (Model_builder.Core.Source.t, Core.Source.t) Formula_entry.public
    | Person of (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public
    | Dance of (Model_builder.Core.Dance.t, Core.Dance.t) Formula_entry.public
    | Book of (Model_builder.Core.Book.t, Core.Book.t) Formula_entry.private_
    | Set of (Model_builder.Core.Set.t, Core.Set.t) Formula_entry.private_
    | Tune of (Model_builder.Core.Tune.t, Core.Tune.t) Formula_entry.public
    | Version of (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public
    | User of (Model_builder.Core.User.t, Core.User.t) Formula_entry.public
    | Raw of string
  (** Type of predicates on “any” elements. *)

  type t = predicate Formula.t
  (** Type of a filter on “any” element, that is a formula over
      {!predicate}s. *)

  (* NOTE: There is no [accepts_any] because the search of Any is handled
     “manually” in the Any controller, for performance reasons. *)

  (** {3 Constructors} *)

  val raw : string -> predicate
  val raw' : string -> t
  (** A filter containing raw strings, semantically equivalent to the
      disjunction of the [raw] cases of all the other models. *)

  val source : (Model_builder.Core.Source.t, Core.Source.t) Formula_entry.public -> predicate
  val source' : (Model_builder.Core.Source.t, Core.Source.t) Formula_entry.public -> t
  (** Lift a filter on sources to make a filter on “any”. This filter asserts
      that the “any” element is a source that matches the given filter. *)

  val person : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public -> predicate
  val person' : (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public -> t
  (** Lift a filter on persons to make a filter on “any”. This filter asserts
      that the “any” element is a person that matches the given filter. *)

  val dance : (Model_builder.Core.Dance.t, Core.Dance.t) Formula_entry.public -> predicate
  val dance' : (Model_builder.Core.Dance.t, Core.Dance.t) Formula_entry.public -> t
  (** Lift a filter on dances to make a filter on “any”. This filter asserts
      that the “any” element is a dance that matches the given filter. *)

  val book : (Model_builder.Core.Book.t, Core.Book.t) Formula_entry.private_ -> predicate
  val book' : (Model_builder.Core.Book.t, Core.Book.t) Formula_entry.private_ -> t
  (** Lift a filter on books to make a filter on “any”. This filter asserts that
      the “any” element is a book that matches the given filter. *)

  val set : (Model_builder.Core.Set.t, Core.Set.t) Formula_entry.private_ -> predicate
  val set' : (Model_builder.Core.Set.t, Core.Set.t) Formula_entry.private_ -> t
  (** Lift a filter on sets to make a filter on “any”. This filter asserts that
      the “any” element is a set that matches the given filter. *)

  val tune : (Model_builder.Core.Tune.t, Core.Tune.t) Formula_entry.public -> predicate
  val tune' : (Model_builder.Core.Tune.t, Core.Tune.t) Formula_entry.public -> t
  (** Lift a filter on tunes to make a filter on “any”. This filter asserts
      that the “any” element is a tune that matches the given filter. *)

  val version : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public -> predicate
  val version' : (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public -> t
  (** Lift a filter on versions to make a filter on “any”. This filter asserts
      that the “any” element is a version that matches the given filter. *)

  val user : (Model_builder.Core.User.t, Core.User.t) Formula_entry.public -> predicate
  val user' : (Model_builder.Core.User.t, Core.User.t) Formula_entry.public -> t
  (** Lift a filter on users to make a filter on “any”. This filter asserts
      that the “any” element is a user that matches the given filter. *)

  (** {3 Others} *)

  val type_to_exact_predicate : Model_builder.Core.Any.Type.t -> predicate
  (** Given a predicate, return the exact type that this predicate's semantics
      have. For instance, for [Source True], this is [Some Source], but for any
      other subformula of [Source _], this is [None]. *)

  val converter : predicate Text_formula_converter.t

  val type_based_cleanup : t -> t
  (** Part of {!optimise} exposed for testing purposes. *)

  val specialise :
    t ->
    (Model_builder.Core.Book.t, Core.Book.t) Formula_entry.private_
    * (Model_builder.Core.Dance.t, Core.Dance.t) Formula_entry.public
    * (Model_builder.Core.Person.t, Core.Person.t) Formula_entry.public
    * (Model_builder.Core.Set.t, Core.Set.t) Formula_entry.private_
    * (Model_builder.Core.Source.t, Core.Source.t) Formula_entry.public
    * (Model_builder.Core.Tune.t, Core.Tune.t) Formula_entry.public
    * (Model_builder.Core.Version.t, Core.Version.t) Formula_entry.public
  (** Given a formula on any model, returns formulas specialised for all models.
      This is basically the commutativity of the union: the semantics of a
      formula on any (which is the union of all models) is the union of the
      semantics of the specialised formulas. *)
end
