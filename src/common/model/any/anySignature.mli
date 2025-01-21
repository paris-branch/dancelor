(** {1 Any} *)

open Nes
open Dancelor_common_database

type t = [%import: AnyCore.t]
(** Type of an “any” element, that is simply a sum type of all the other
    models. *)

(** {3 Constructors} *)

val person : PersonCore.t Entry.t -> t
(** Function equivalent of the [Person] constructor. *)

val dance : DanceCore.t Entry.t -> t
(** Function equivalent of the [Dance] constructor. *)

val book : BookCore.t Entry.t -> t
(** Function equivalent of the [Book] constructor. *)

val set : SetCore.t Entry.t -> t
(** Function equivalent of the [Set] constructor. *)

val tune : TuneCore.t Entry.t -> t
(** Function equivalent of the [Tune] constructor. *)

val version : VersionCore.t Entry.t -> t
(** Function equivalent of the [Version] constructor. *)

(** {3 Destructors} *)

val equal : t -> t -> bool
(** Equality between two {!t}. *)

val name : t -> string Lwt.t
(** Finds a name to give to the element, no matter what it is. *)

(** {2 Type} *)

module Type : sig
  type t = [%import: AnyCore.Type.t]
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
  type predicate = [%import: AnyCore.Filter.predicate]
  (** Type of predicates on “any” elements. *)

  type t = [%import: AnyCore.Filter.t]
  [@@deriving eq, show]
  (** Type of a filter on “any” element, that is a formula over
      {!predicate}s. *)

  val accepts : t -> AnyCore.t -> float Lwt.t
  (** Whether the given filter accepts the given element. *)

  (** {3 Constructors} *)

  val raw : string -> predicate
  val raw' : string -> t
  (** A filter containing raw strings, semantically equivalent to the
      disjunction of the [raw] cases of all the other models. *)

  val type_ : Type.t -> predicate
  val type_' : Type.t -> t
  (** A filter that asserts that the element has the given type. *)

  val person : PersonSignature.Filter.t -> predicate
  val person' : PersonSignature.Filter.t -> t
  (** Lift a filter on persons to make a filter on “any”. This filter asserts
      that the “any” element is a person that matches the given filter. *)

  val dance : DanceSignature.Filter.t -> predicate
  val dance' : DanceSignature.Filter.t -> t
  (** Lift a filter on dances to make a filter on “any”. This filter asserts
      that the “any” element is a dance that matches the given filter. *)

  val book : BookSignature.Filter.t -> predicate
  val book' : BookSignature.Filter.t -> t
  (** Lift a filter on books to make a filter on “any”. This filter asserts that
      the “any” element is a book that matches the given filter. *)

  val set : SetSignature.Filter.t -> predicate
  val set' : SetSignature.Filter.t -> t
  (** Lift a filter on sets to make a filter on “any”. This filter asserts that
      the “any” element is a set that matches the given filter. *)

  val tune : TuneSignature.Filter.t -> predicate
  val tune' : TuneSignature.Filter.t -> t
  (** Lift a filter on tunes to make a filter on “any”. This filter asserts that
      the “any” element is a tune that matches the given filter. *)

  val version : VersionSignature.Filter.t -> predicate
  val version' : VersionSignature.Filter.t -> t
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

(** {2 Search} *)

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the objects
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of objects. The second element of the pair
    is a slice of the list, taken as per the [slice] (if any). *)

val search' :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  t list Lwt.t
(** Like {!val-search} but returns only the list. *)

val search_context :
  ?threshold: float ->
  Filter.t ->
  t ->
  (int * t option * int * t option) Lwt.t
(** [search_context ?threshold filter elt] is equivalent to running [search
    ?threshold filter], looking for [elt] in the resulting list and returning
    the total number of elements, the element before [elt], the index of [elt],
    and the element after [elt], but it does this much more efficiently. *)

val count :
  ?threshold: float ->
  Filter.t ->
  int Lwt.t
(** Like {!val-search} but returns only the number of items. *)
