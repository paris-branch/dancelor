module type S = sig
  (** {1 Person}

      This module represents a “person”, in the wide definition. A person can be
      an actual human person (eg. “William Marshall”), a group of people (eg. “The
      Craigellachie Band”) or an abstract notion (eg. “Traditional” or “RSCDS”).
      This is a notion similar to that of the SCDDB. *)

  open Nes
  open Core

  type t = Person.t
  (** Abstract type for a person. *)

  val make : name: string -> ?scddb_id: int -> unit -> t

  (** {2 Field getters} *)

  val name : t Entry.t -> string
  val scddb_id : t Entry.t -> int option

  val is_trad : t Entry.t -> bool

  val equal : t -> t -> bool

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Person.predicate
    type t = Filter.Person.t

    val accepts : t -> Person.t Entry.t -> float Lwt.t

    val is : Person.t Entry.t -> predicate
    val is' : Person.t Entry.t -> t

    val text_formula_converter : predicate TextFormulaConverter.t
    val from_text_formula : TextFormula.t -> (t, string) Result.t
    val from_string : ?filename: string -> string -> (t, string) Result.t
    val to_string : t -> string

    val optimise : t -> t
  end

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Slug.t -> t Entry.t Lwt.t
end
