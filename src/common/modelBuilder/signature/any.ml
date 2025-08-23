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
    | User of User.t Entry.t
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

  val user : User.t Entry.t -> t
  (** Function equivalent of the [User] constructor. *)

  (** {3 Destructors} *)

  val equal : t -> t -> bool
  (** Equality between two {!t}. *)

  val name : t -> NEString.t Lwt.t
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
      | User
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

    val equal : t -> t -> bool
    (** Equality between two types. *)
  end

  val type_of : t -> Type.t
  (** Get the type of an “any” element. *)

  val to_entry : t -> unit Entry.t
  (** Cast an {!Any.t} to an entry. Of course, the value then needs to be forgotten. *)
end
