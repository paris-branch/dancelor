module type S = sig
  (** {1 Any filter} *)

  type predicate = Core.Any.predicate =
    | Raw of string
    | Type of ModelBuilder.Core.Any.Type.t
    (* lifting predicates: *)
    | Source of Core.Source.t
    | Person of Core.Person.t
    | Dance of Core.Dance.t
    | Book of Core.Book.t
    | Set of Core.Set.t
    | Tune of Core.Tune.t
    | Version of Core.Version.t
  (** Type of predicates on “any” elements. *)

  type t = predicate Formula.t
  (** Type of a filter on “any” element, that is a formula over
      {!predicate}s. *)

  val accepts : t -> ModelBuilder.Core.Any.t -> float Lwt.t
  (** Whether the given filter accepts the given element. *)

  (** {3 Constructors} *)

  val raw : string -> predicate
  val raw' : string -> t
  (** A filter containing raw strings, semantically equivalent to the
      disjunction of the [raw] cases of all the other models. *)

  val type_ : ModelBuilder.Core.Any.Type.t -> predicate
  val type_' : ModelBuilder.Core.Any.Type.t -> t
  (** A filter that asserts that the element has the given type. *)

  val source : Core.Source.t -> predicate
  val source' : Core.Source.t -> t
  (** Lift a filter on sources to make a filter on “any”. This filter asserts
      that the “any” element is a source that matches the given filter. *)

  val person : Core.Person.t -> predicate
  val person' : Core.Person.t -> t
  (** Lift a filter on persons to make a filter on “any”. This filter asserts
      that the “any” element is a person that matches the given filter. *)

  val dance : Core.Dance.t -> predicate
  val dance' : Core.Dance.t -> t
  (** Lift a filter on dances to make a filter on “any”. This filter asserts
      that the “any” element is a dance that matches the given filter. *)

  val book : Core.Book.t -> predicate
  val book' : Core.Book.t -> t
  (** Lift a filter on books to make a filter on “any”. This filter asserts that
      the “any” element is a book that matches the given filter. *)

  val set : Core.Set.t -> predicate
  val set' : Core.Set.t -> t
  (** Lift a filter on sets to make a filter on “any”. This filter asserts that
      the “any” element is a set that matches the given filter. *)

  val tune : Core.Tune.t -> predicate
  val tune' : Core.Tune.t -> t
  (** Lift a filter on tunes to make a filter on “any”. This filter asserts that
      the “any” element is a tune that matches the given filter. *)

  val version : Core.Version.t -> predicate
  val version' : Core.Version.t -> t
  (** Lift a filter on versions to make a filter on “any”. This filter asserts
      that the “any” element is a version that matches the given filter. *)

  (** {3 Destructors} *)

  val from_string : ?filename: string -> string -> (t, string) Result.t
  (** Parse a text formula into a filter on “any” elements. *)

  val to_string : t -> string
  (** Convert a formula on “any” elements into a text formula representing
      it. *)

  val to_pretty_string : t -> string
  (** Convert a formula on “any” elements into an equivalent text formula meant
      to be more readable to humans. *)

  (** {3 Others} *)

  val optimise : t -> t
  (** Optimise a filter of “any” elements. This relies on the generic
      {!Formula.optimise} but it also merges predicates together; for instance,
      ["type:Version version:<vfilter1> version:<vfilter2>"] will be optimised
      as ["version:(<vfilter1> <vfilter2>)"]. *)

  val type_based_cleanup : t -> t
  (** Part of {!optimise} exposed for testing purposes. *)
end
