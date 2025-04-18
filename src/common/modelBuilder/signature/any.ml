module type S = sig
  (** {1 Any} *)

  open Nes
  open Core

  type t = Any.t =
    | Source of Source.t Entry.t
    | Person of Person.t Entry.t
    | Dance of Dance.t Entry.t
    | Book of Book.t Entry.t
    | Set of Set.t Entry.t
    | Tune of Tune.t Entry.t
    | Version of Version.t Entry.t
  (** Type of an “any” element, that is simply a sum type of all the other
      models. *)

  (** {3 Constructors} *)

  val source : Source.t Entry.t -> t
  (** Function equivalent of the [Source] constructor. *)

  val person : Person.t Entry.t -> t
  (** Function equivalent of the [Person] constructor. *)

  val dance : Dance.t Entry.t -> t
  (** Function equivalent of the [Dance] constructor. *)

  val book : Book.t Entry.t -> t
  (** Function equivalent of the [Book] constructor. *)

  val set : Set.t Entry.t -> t
  (** Function equivalent of the [Set] constructor. *)

  val tune : Tune.t Entry.t -> t
  (** Function equivalent of the [Tune] constructor. *)

  val version : Version.t Entry.t -> t
  (** Function equivalent of the [Version] constructor. *)

  (** {3 Destructors} *)

  val equal : t -> t -> bool
  (** Equality between two {!t}. *)

  val name : t -> string Lwt.t
  (** Finds a name to give to the element, no matter what it is. *)

  (** {2 Type} *)

  module Type : sig
    type t = Core.Any.Type.t =
      | Source
      | Person
      | Dance
      | Book
      | Set
      | Tune
      | Version
    (** Type to represent the type of an “any”. There is basically one type per
        model, eg. [Version] or [Dance]. Must not be mistaken for a kind, which,
        in Dancelor parlance, is eg. [Reel] or [8 x 32 Strathspey]. *)

    val all : t list
    (** All the existing types, as a list. There is no guarantee on the order in
        which those elements appear. *)

    val are_all : t list -> bool
    (** Whether the given list contains all the existing types. *)

    val compare : t -> t -> int
    (** Total comparison function for types. *)

    val to_string : t -> string
    (** Convert a type to a string, eg [to_string Person = "Person"]. *)

    exception NotAType of string
    (** See {!of_string}. *)

    val of_string : string -> t
    (** Convert a string to a type, eg. [of_string "person" = Person]. If the
        string is not a valid representation of a type, raises {!NotAType}. *)

    val of_string_opt : string -> t option
    (** Option equivalent of {!of_string}. *)
  end

  val type_of : t -> Type.t
  (** Get the type of an “any” element. *)

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Any.predicate =
      | Raw of string
      | Type of Core.Any.Type.t
      (* lifting predicates: *)
      | Source of Filter.Source.t
      | Person of Filter.Person.t
      | Dance of Filter.Dance.t
      | Book of Filter.Book.t
      | Set of Filter.Set.t
      | Tune of Filter.Tune.t
      | Version of Filter.Version.t
    (** Type of predicates on “any” elements. *)

    type t = predicate Formula.t
    (** Type of a filter on “any” element, that is a formula over
        {!predicate}s. *)

    val accepts : t -> Core.Any.t -> float Lwt.t
    (** Whether the given filter accepts the given element. *)

    (** {3 Constructors} *)

    val raw : string -> predicate
    val raw' : string -> t
    (** A filter containing raw strings, semantically equivalent to the
        disjunction of the [raw] cases of all the other models. *)

    val type_ : Type.t -> predicate
    val type_' : Type.t -> t
    (** A filter that asserts that the element has the given type. *)

    val source : Filter.Source.t -> predicate
    val source' : Filter.Source.t -> t
    (** Lift a filter on sources to make a filter on “any”. This filter asserts
        that the “any” element is a source that matches the given filter. *)

    val person : Filter.Person.t -> predicate
    val person' : Filter.Person.t -> t
    (** Lift a filter on persons to make a filter on “any”. This filter asserts
        that the “any” element is a person that matches the given filter. *)

    val dance : Filter.Dance.t -> predicate
    val dance' : Filter.Dance.t -> t
    (** Lift a filter on dances to make a filter on “any”. This filter asserts
        that the “any” element is a dance that matches the given filter. *)

    val book : Filter.Book.t -> predicate
    val book' : Filter.Book.t -> t
    (** Lift a filter on books to make a filter on “any”. This filter asserts that
        the “any” element is a book that matches the given filter. *)

    val set : Filter.Set.t -> predicate
    val set' : Filter.Set.t -> t
    (** Lift a filter on sets to make a filter on “any”. This filter asserts that
        the “any” element is a set that matches the given filter. *)

    val tune : Filter.Tune.t -> predicate
    val tune' : Filter.Tune.t -> t
    (** Lift a filter on tunes to make a filter on “any”. This filter asserts that
        the “any” element is a tune that matches the given filter. *)

    val version : Filter.Version.t -> predicate
    val version' : Filter.Version.t -> t
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
end
