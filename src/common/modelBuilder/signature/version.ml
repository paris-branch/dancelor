module type S = sig
  open Nes
  open Core

  type t = Version.t

  val make :
    tune: Tune.t Entry.t ->
    bars: int ->
    key: Music.key ->
    structure: string ->
    ?sources: Source.t Entry.t list ->
    ?arrangers: Person.t Entry.t list ->
    ?remark: string ->
    ?disambiguation: string ->
    content: string ->
    unit ->
    t

  val tune : t Entry.t -> Tune.t Entry.t Lwt.t
  val bars : t Entry.t -> int
  val key : t Entry.t -> Music.key
  val structure : t Entry.t -> string
  val sources : t Entry.t -> Source.t Entry.t list Lwt.t
  val arrangers : t Entry.t -> Person.t Entry.t list Lwt.t
  val remark : t Entry.t -> string
  val disambiguation : t Entry.t -> string
  val content : t Entry.t -> string

  val kind : t Entry.t -> Kind.Version.t Lwt.t
  (** Convenient wrapper around {!bars} and {!Tune.kind}. *)

  val name : t Entry.t -> string Lwt.t
  (** Convenient wrapper around {!tune} and {!Tune.name}. *)

  val equal : t -> t -> bool

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Version.predicate
    type t = Filter.Version.t

    val accepts : t -> Version.t Entry.t -> float Lwt.t

    val is : Version.t Entry.t -> predicate
    val is' : Version.t Entry.t -> t

    val tuneIs : Tune.t Entry.t -> predicate
    val tuneIs' : Tune.t Entry.t -> t

    val tune : Filter.Tune.t -> predicate
    val tune' : Filter.Tune.t -> t

    val kind : Kind.Version.Filter.t -> predicate
    val kind' : Kind.Version.Filter.t -> t

    val key : Music.Key.t -> predicate
    val key' : Music.Key.t -> t

    val existsSource : Filter.Source.t -> predicate
    val existsSource' : Filter.Source.t -> t

    val memSource : Source.t Entry.t -> predicate
    val memSource' : Source.t Entry.t -> t

    val text_formula_converter : predicate TextFormulaConverter.t
    val from_text_formula : TextFormula.t -> (t, string) Result.t
    val from_string : ?filename: string -> string -> (t, string) Result.t
    val to_string : t -> string

    val optimise : t -> t
  end

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Entry.t Lwt.t

  val create : t -> t Entry.t Lwt.t
  (** Create a new database entry for the given version. *)

  val update : t Slug.t -> t -> t Entry.t Lwt.t
  (** Update an existing database entry with the given version. *)

  val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
  (** Either {!create} or {!update}. *)

  val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
  (** Returns the list of all the versions that match the filter with a score
      higher than the hardcoded threshold. The first element of the pair is the
      number of versions. The second element of the pair is a slice of the list,
      taken as per the slice. *)

  val search' : Filter.t -> t Entry.t list Lwt.t
  (** Like {!search} but returns only the list of all values. *)

  val count : Filter.t -> int Lwt.t
  (** Like {!search} but returns only the number of items. *)
end
