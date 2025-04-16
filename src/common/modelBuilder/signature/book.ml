module type S = sig
  (** {1 Book Signature}

      This module contains the signature of books, shared by both Dancelor's client
      and server. On the server side, some of these functions involve database
      accesses; on the client side, network calls. *)

  open Nes
  open Core

  (** {2 Types} *)

  type page = Book.page =
    | Version of Version.t Entry.t * VersionParameters.t
    | Set of Set.t Entry.t * SetParameters.t
    | InlineSet of Set.t * SetParameters.t
  (** The type of one page in a book. A page either consists of a version (eg.
      in a book of tunes), or a set (eg. in a dance program) or a so-called
      “inline set”. Inline sets are simply a way to define a set on-the-fly in
      the database for books without actually giving it a corresponding set
      entry. It can be useful to put several versions on the same page, for
      instance, when they do not particularly make sense together. *)

  type t = Book.t
  (** The type of a book. Even if it is known that it is a record, it should never
      be manipulated explicitly. *)

  val make :
    title: string ->
    ?subtitle: string ->
    ?short_title: string ->
    ?date: PartialDate.t ->
    ?contents: page list ->
    ?source: bool ->
    ?remark: string ->
    ?scddb_id: int ->
    unit ->
    t

  (** {2 Field Getters} *)

  val title : t Entry.t -> string
  val subtitle : t Entry.t -> string
  val short_title : t Entry.t -> string
  val date : t Entry.t -> PartialDate.t option
  val contents : t Entry.t -> page list Lwt.t
  val source : t Entry.t -> bool
  val remark : t Entry.t -> string
  val scddb_id : t Entry.t -> int option

  (** {2 Advanced Field Getters} *)

  val is_source : t Entry.t -> bool

  val versions_from_contents : t Entry.t -> Version.t Entry.t list Lwt.t
  (** Extract only the versions from the book's contents. *)

  val sets_from_contents : t Entry.t -> Set.t Entry.t list Lwt.t
  (** Extract the sets (both normal and inline) from the book's contents. *)

  val unique_sets_from_contents : t Entry.t -> Set.t Entry.t list Lwt.t
  (** Same as {!sets_from_contents} but without duplicate sets. *)

  val sets_and_parameters_from_contents : t Entry.t -> (Set.t Entry.t * SetParameters.t) list Lwt.t
  (** Same as {!sets_from_contents} but also includes parameters. *)

  val find_context_no_inline : int -> t Entry.t -> page List.context option Lwt.t
  (** Given an indice and a book, find the context around that indice in the book.
      Ignores the [InlineSet] pages, both in the given indice and the resulting
      context. *)

  (** {2 Utilities} *)

  val contains_set : Set.t Slug.t -> t Entry.t -> bool
  val compare : t Entry.t -> t Entry.t -> int
  val equal : t Entry.t -> t Entry.t -> bool

  val lilypond_contents_cache_key : t Entry.t -> string Lwt.t

  (** {2 Warnings} *)

  type warning = Book.warning =
    | Empty
    | DuplicateSet of Set.t Entry.t
    | DuplicateVersion of Tune.t Entry.t * (Set.t Entry.t option * int) list
    | SetDanceMismatch of Set.t Entry.t * Dance.t Entry.t
  (* FIXME: a more specific type for (Set.t option * int) list. Maybe
     “occurrences”? And maybe with a record so that this “int” has a name? *)

  type warnings = warning list

  val warnings : t Entry.t -> warnings Lwt.t

  (** {2 Filters} *)

  module Filter : sig
    type predicate = Filter.Book.predicate
    type t = Filter.Book.t

    val accepts : t -> Book.t Entry.t -> float Lwt.t

    val isSource : predicate
    val isSource' : t

    val memSet : Set.t Entry.t -> predicate
    val memSet' : Set.t Entry.t -> t

    val memTuneDeep' : Tune.t Entry.t -> t
    (** Matches if the given tune appears in any version at any depth in the book,
        that is directly in the book or in a set of the book. *)

    val memVersionDeep' : Version.t Entry.t -> t
    (** Matches if the given version appears at any depth in the book, that is
        directly in the book or in a set of the book. *)

    val existsTuneDeep' : Filter.Tune.t -> t
    val existsVersionDeep' : Filter.Version.t -> t

    val text_formula_converter : predicate TextFormulaConverter.t
    val from_text_formula : TextFormula.t -> (t, string) Result.t
    val from_string : ?filename: string -> string -> (t, string) Result.t
    val to_string : t -> string

    val optimise : t -> t
  end

  val page_core_to_page : Book.Page.t -> page Lwt.t
  (** Exposed for use in the book controller. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Slug.t -> t Entry.t Lwt.t
end
