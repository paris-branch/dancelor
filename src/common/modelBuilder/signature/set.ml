module type S = sig
  open Nes
  open Core

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

  val name : t -> string
  val name' : t Entry.t -> string

  val conceptors : t -> Person.t Entry.t list Lwt.t
  val conceptors' : t Entry.t -> Person.t Entry.t list Lwt.t

  val kind : t -> Kind.Dance.t
  val kind' : t Entry.t -> Kind.Dance.t

  val contents : t -> (Version.t Entry.t * VersionParameters.t) list Lwt.t
  val contents' : t Entry.t -> (Version.t Entry.t * VersionParameters.t) list Lwt.t

  val order : t -> SetOrder.t
  val order' : t Entry.t -> SetOrder.t

  val instructions : t -> string
  val instructions' : t Entry.t -> string

  val dances : t -> Dance.t Entry.t list Lwt.t
  val dances' : t Entry.t -> Dance.t Entry.t list Lwt.t

  val remark : t -> string
  val remark' : t Entry.t -> string

  val contains_version : Version.t Slug.t -> t Entry.t -> bool
  (** REVIEW: This really takes a slug? *)

  val find_context : int -> t -> Version.t Entry.t List.context option Lwt.t
  val find_context' : int -> t Entry.t -> Version.t Entry.t List.context option Lwt.t
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

  val warnings : t -> warnings Lwt.t
  val warnings' : t Entry.t -> warnings Lwt.t

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Set.predicate
    type t = Filter.Set.t

    val accepts : t -> Set.t Entry.t -> float Lwt.t

    val is : Set.t Entry.t -> predicate
    val is' : Set.t Entry.t -> t

    val existsVersion : Filter.Version.t -> predicate
    val existsVersion' : Filter.Version.t -> t

    val existsConceptor : Filter.Person.t -> predicate
    val existsConceptor' : Filter.Person.t -> t

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

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Slug.t -> t Entry.t Lwt.t
end
