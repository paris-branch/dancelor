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

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Entry.t Lwt.t
  (** Look up a source in the database given its slug. On the client-side, this
      involves an API call. *)

  val create : t -> t Entry.t Lwt.t
  (** Create a new database entry for the given source. *)

  val update : t Slug.t -> t -> t Entry.t Lwt.t
  (** Update an existing database entry with the given source. *)

  val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
  (** Either {!create} or {!update}. *)

  val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
  (** Returns the list of all the sources that match the filter with a score
      higher than the hardcoded threshold. The first element of the pair is the
      number of sources. The second element of the pair is a slice of the list,
      taken as per the slice. *)

  val search' : Filter.t -> t Entry.t list Lwt.t
  (** Like {!search} but returns only the list of all values. *)

  val count : Filter.t -> int Lwt.t
  (** Like {!search} but returns only the number of items. *)
end
