module type S = sig
  open Nes
  open Dancelor_common_database
  open Dancelor_common_model_utils
  open Dancelor_common_model_core

  type t = Set.t

  val make :
    name: string ->
    ?conceptors: Person.t Entry.t list ->
    kind: Kind.Dance.t ->
    ?contents: (Version.t Entry.t * VersionParameters.t) list ->
    order: SetOrder.t ->
    ?dances: Dance.t Entry.t list ->
    unit ->
    t

  (* FIXME: remove? *)
  val is_slug_none : t Entry.t -> bool

  val name : t Entry.t -> string
  val conceptors : t Entry.t -> Person.t Entry.t list Lwt.t
  val kind : t Entry.t -> Kind.Dance.t
  val contents : t Entry.t -> (Version.t Entry.t * VersionParameters.t) list Lwt.t
  val order : t Entry.t -> SetOrder.t
  val instructions : t Entry.t -> string
  val dances : t Entry.t -> Dance.t Entry.t list Lwt.t
  val remark : t Entry.t -> string

  val contains_version : Version.t Slug.t -> t Entry.t -> bool
  (** REVIEW: This really takes a slug? *)

  val find_context : int -> t Entry.t -> Version.t Entry.t List.context option Lwt.t
  (** Given an indice and a set, find the context around that indice in the
      set. *)

  val compare : t Entry.t -> t Entry.t -> int
  val equal : t Entry.t -> t Entry.t -> bool

  val lilypond_content_cache_key : t Entry.t -> string Lwt.t

  (* {2 Warnings} *)

  type warning = Set.warning =
    | Empty
    | WrongKind
    | WrongVersionBars of Version.t Entry.t
    | WrongVersionKind of Tune.t Entry.t
    | DuplicateVersion of Tune.t Entry.t

  type warnings = warning list

  val warnings : t Entry.t -> warnings Lwt.t

  (** {2 Filters} *)

  module Filter : sig
    type predicate = [%import: Dancelor_common_model_filter.Set.predicate]
    type t = [%import: Dancelor_common_model_filter.Set.t]
    [@@deriving eq, show]

    val accepts : t -> Set.t Entry.t -> float Lwt.t

    val is : Set.t Entry.t -> predicate
    val is' : Set.t Entry.t -> t

    val existsVersion : Dancelor_common_model_filter.Version.t -> predicate
    val existsVersion' : Dancelor_common_model_filter.Version.t -> t

    val existsConceptor : Dancelor_common_model_filter.Person.t -> predicate
    val existsConceptor' : Dancelor_common_model_filter.Person.t -> t

    val kind : KindDance.Filter.t -> predicate
    val kind' : KindDance.Filter.t -> t

    val memVersion : Version.t Entry.t -> predicate
    val memVersion' : Version.t Entry.t -> t

    val text_formula_converter : predicate TextFormulaConverter.t
    val from_text_formula : TextFormula.t -> (t, string) Result.t
    val from_string : ?filename: string -> string -> (t, string) Result.t
    val to_string : t -> string

    val optimise : t -> t
  end

  (** {2 Getters and setters} *)

  val get : t Slug.t -> t Entry.t Lwt.t

  val create : t -> t Entry.t Lwt.t
  (** Create a new database entry for the given set. *)

  val update : t Slug.t -> t -> t Entry.t Lwt.t
  (** Update an existing database entry with the given set. *)

  val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
  (** Either {!create} or {!update}. *)

  val delete : t Entry.t -> unit Lwt.t

  val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
  (** Returns the list of all the sets that match the filter with a score higher
      than the hardcoded threshold. The first element of the pair is the number of
      sets. The second element of the pair is a slice of the list, taken as per
      the slice. *)

  val search' : Filter.t -> t Entry.t list Lwt.t
  (** Like {!search} but returns only the list of all values. *)

  val count : Filter.t -> int Lwt.t
  (** Like {!search} but returns only the number of items. *)
end
