module type S = sig
  (** {1 Dance} *)

  open Nes
  open Core

  type t = Dance.t

  val make :
    name: string ->
    kind: Kind.Dance.t ->
    ?devisers: Person.t Entry.t list ->
    ?two_chords: bool ->
    ?scddb_id: int ->
    ?disambiguation: string ->
    ?date: PartialDate.t ->
    unit ->
    t

  (** {2 Field getters} *)

  val name : t Entry.t -> string
  val kind : t Entry.t -> Kind.Dance.t
  val devisers : t Entry.t -> Person.t Entry.t list Lwt.t
  val two_chords : t Entry.t -> bool option
  val scddb_id : t Entry.t -> int option
  val disambiguation : t Entry.t -> string
  val date : t Entry.t -> PartialDate.t option

  val equal : t Entry.t -> t Entry.t -> bool

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Dance.predicate
    type t = Filter.Dance.t
    [@@deriving eq, show]

    val accepts : t -> Dance.t Entry.t -> float Lwt.t

    val is : Dance.t Entry.t -> predicate
    val is' : Dance.t Entry.t -> t

    val kind : KindDance.Filter.t -> predicate
    val kind' : KindDance.Filter.t -> t

    val existsDeviser : Filter.Person.t -> predicate
    val existsDeviser' : Filter.Person.t -> t

    val text_formula_converter : predicate TextFormulaConverter.t
    val from_text_formula : TextFormula.t -> (t, string) Result.t
    val from_string : ?filename: string -> string -> (t, string) Result.t
    val to_string : t -> string

    val optimise : t -> t
  end

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Entry.t Lwt.t

  val create : t -> t Entry.t Lwt.t
  (** Create a new database entry for the given dance. *)

  val update : t Slug.t -> t -> t Entry.t Lwt.t
  (** Update an existing database entry with the given dance. *)

  val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
  (** Either {!create} or {!update}. *)

  val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
  (** Returns the list of all the dances that match the filter with a score higher
      than the hardcoded threshold. The first element of the pair is the number of
      dances. The second element of the pair is a slice of the list, taken as per
      the slice. *)

  val search' : Filter.t -> t Entry.t list Lwt.t
  (** Like {!search} but returns only the list of all values. *)

  val count : Filter.t -> int Lwt.t
  (** Like {!search} but returns only the number of items. *)
end
