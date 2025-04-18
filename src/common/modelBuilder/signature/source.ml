module type S = sig
  (** {1 Source}

      This module represents a “source”, that is typically a publication
      containing tunes, and with which we can check the correspondance. *)

  open Nes
  open Core

  type t = Source.t
  (** Abstract type for a source. *)

  val make :
    name: string ->
    ?scddb_id: int ->
    ?description: string ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t Entry.t -> string
  val scddb_id : t Entry.t -> int option
  val description : t Entry.t -> string option

  val equal : t -> t -> bool

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Source.predicate
    type t = Filter.Source.t

    val accepts : t -> Source.t Entry.t -> float Lwt.t

    val is : Source.t Entry.t -> predicate
    val is' : Source.t Entry.t -> t

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
