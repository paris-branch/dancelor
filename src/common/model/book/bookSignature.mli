(** {1 Book Signature}

    This module contains the signature of books, shared by both Dancelor's client
    and server. On the server side, some of these functions involve database
    accesses; on the client side, network calls. *)

open Nes

(** {2 Types} *)

type page = BookCore.page =
  | Version of VersionCore.t * VersionParameters.t
  | Set of SetCore.t * SetParameters.t
  | InlineSet of SetCore.core * SetParameters.t
  (** The type of one page in a book. A page either consists of a version (eg.
      in a book of tunes), or a set (eg. in a dance program) or a so-called
      “inline set”. Inline sets are simply a way to define a set on-the-fly in
      the database for books without actually giving it a corresponding set
      entry. It can be useful to put several versions on the same page, for
      instance, when they do not particularly make sense together. *)

type t = BookCore.t
(** The type of a book. Even if it is known that it is a record, it should never
    be manipulated explicitly. *)

(** {2 Field Getters} *)

val title : t -> string
val subtitle : t -> string
val short_title : t -> string
val date : t -> PartialDate.t option
val contents : t -> page list Lwt.t
val source : t -> bool
val remark : t -> string
val scddb_id : t -> int option

(** {2 Advanced Field Getters} *)

val is_source : t -> bool

val versions_from_contents : t -> VersionCore.t list Lwt.t
(** Extract only the versions from the book's contents. *)

val sets_from_contents : t -> SetCore.t list Lwt.t
(** Extract the sets (both normal and inline) from the book's contents. *)

val unique_sets_from_contents : t -> SetCore.t list Lwt.t
(** Same as {!sets_from_contents} but without duplicate sets. *)

val sets_and_parameters_from_contents : t -> (SetCore.t * SetParameters.t) list Lwt.t
(** Same as {!sets_from_contents} but also includes parameters. *)

val find_context_no_inline : int -> t -> page List.context option Lwt.t
(** Given an indice and a book, find the context around that indice in the book.
    Ignores the [InlineSet] pages, both in the given indice and the resulting
    context. *)

(** {2 Utilities} *)

val contains_set : SetCore.t Slug.t -> t -> bool
val compare : t -> t -> int
val equal : t -> t -> bool

val lilypond_contents_cache_key : t -> string Lwt.t

(** {2 Warnings} *)

type warning = BookCore.warning =
  | Empty
  | DuplicateSet of SetCore.t
  | DuplicateVersion of TuneCore.t * (SetCore.t option * int) list
  | SetDanceMismatch of SetCore.t * DanceCore.t
  (* FIXME: a more specific type for (SetCore.t option * int) list. Maybe
     “occurrences”? And maybe with a record so that this “int” has a name? *)

type warnings = warning list

val warnings : t -> warnings Lwt.t

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: BookCore.Filter.predicate]
  type t = [%import: BookCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> BookCore.t -> float Lwt.t

  val isSource : predicate
  val isSource' : t

  val memSet : SetCore.t -> predicate
  val memSet' : SetCore.t -> t

  val memTuneDeep' : TuneCore.t -> t
  (** Matches if the given tune appears in any version at any depth in the book,
      that is directly in the book or in a set of the book. *)

  val memVersionDeep' : VersionCore.t -> t
  (** Matches if the given version appears at any depth in the book, that is
      directly in the book or in a set of the book. *)

  val existsTuneDeep' : TuneCore.Filter.t -> t
  val existsVersionDeep' : VersionCore.Filter.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end

(** {2 API Getters & Setters} *)

val get : t Slug.t -> t Lwt.t

val save :
  ?status: Dancelor_common_database.Status.t ->
  title: string ->
  ?date: PartialDate.t ->
  ?contents: page list ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Lwt.t

val make :
  ?status: Dancelor_common_database.Status.t ->
  slug: t Slug.t ->
  title: string ->
  ?date: PartialDate.t ->
  ?contents: page list ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Lwt.t
(** Low-level unsafe book creation. Prefer {!save} or {!update}. *)

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the books
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of books. The second element of the pair
    is a slice of the list, taken as per the [slice] (if any). *)

val search' :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  t list Lwt.t
(** Like {!search} but returns only the list. *)

val count :
  ?threshold: float ->
  Filter.t ->
  int Lwt.t
(** Like {!search} but returns only the number of items. *)

val update :
  ?status: Dancelor_common_database.Status.t ->
  slug: t Slug.t ->
  title: string ->
  ?date: PartialDate.t ->
  ?contents: page list ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  unit Lwt.t

val page_core_to_page : BookCore.PageCore.t -> page Lwt.t
(** Exposed for use in the book controller. *)
