(** {1 Book Signature}

    This module contains the signature of books, shared by both Dancelor's client
    and server. On the server side, some of these functions involve database
    accesses; on the client side, network calls. *)

open Nes
open Dancelor_common_database

(** {2 Types} *)

type page = BookCore.page =
  | Version of VersionCore.t Entry.t * VersionParameters.t
  | Set of SetCore.t Entry.t * SetParameters.t
  | InlineSet of SetCore.t * SetParameters.t
  (** The type of one page in a book. A page either consists of a version (eg.
      in a book of tunes), or a set (eg. in a dance program) or a so-called
      “inline set”. Inline sets are simply a way to define a set on-the-fly in
      the database for books without actually giving it a corresponding set
      entry. It can be useful to put several versions on the same page, for
      instance, when they do not particularly make sense together. *)

type t = BookCore.t
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

val versions_from_contents : t Entry.t -> VersionCore.t Entry.t list Lwt.t
(** Extract only the versions from the book's contents. *)

val sets_from_contents : t Entry.t -> SetCore.t Entry.t list Lwt.t
(** Extract the sets (both normal and inline) from the book's contents. *)

val unique_sets_from_contents : t Entry.t -> SetCore.t Entry.t list Lwt.t
(** Same as {!sets_from_contents} but without duplicate sets. *)

val sets_and_parameters_from_contents : t Entry.t -> (SetCore.t Entry.t * SetParameters.t) list Lwt.t
(** Same as {!sets_from_contents} but also includes parameters. *)

val find_context_no_inline : int -> t Entry.t -> page List.context option Lwt.t
(** Given an indice and a book, find the context around that indice in the book.
    Ignores the [InlineSet] pages, both in the given indice and the resulting
    context. *)

(** {2 Utilities} *)

val contains_set : SetCore.t Slug.t -> t Entry.t -> bool
val compare : t Entry.t -> t Entry.t -> int
val equal : t Entry.t -> t Entry.t -> bool

val lilypond_contents_cache_key : t Entry.t -> string Lwt.t

(** {2 Warnings} *)

type warning = BookCore.warning =
  | Empty
  | DuplicateSet of SetCore.t Entry.t
  | DuplicateVersion of TuneCore.t Entry.t * (SetCore.t Entry.t option * int) list
  | SetDanceMismatch of SetCore.t Entry.t * DanceCore.t Entry.t
  (* FIXME: a more specific type for (SetCore.t option * int) list. Maybe
     “occurrences”? And maybe with a record so that this “int” has a name? *)

type warnings = warning list

val warnings : t Entry.t -> warnings Lwt.t

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: BookCore.Filter.predicate]
  type t = [%import: BookCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> BookCore.t Entry.t -> float Lwt.t

  val isSource : predicate
  val isSource' : t

  val memSet : SetCore.t Entry.t -> predicate
  val memSet' : SetCore.t Entry.t -> t

  val memTuneDeep' : TuneCore.t Entry.t -> t
  (** Matches if the given tune appears in any version at any depth in the book,
      that is directly in the book or in a set of the book. *)

  val memVersionDeep' : VersionCore.t Entry.t -> t
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

val get : t Slug.t -> t Entry.t Lwt.t

val create : t -> t Entry.t Lwt.t
(** Create a new database entry for the given book. *)

val update : t Slug.t -> t -> t Entry.t Lwt.t
(** Update an existing database entry with the given book. *)

val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
(** Either {!create} or {!update}. *)

val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
(** Returns the list of all the books that match the filter with a score higher
    than the hardcoded threshold. The first element of the pair is the number of
    books. The second element of the pair is a slice of the list, taken as per
    the slice. *)

val search' : Filter.t -> t Entry.t list Lwt.t
(** Like {!search} but returns only the list of all values. *)

val count : Filter.t -> int Lwt.t
(** Like {!search} but returns only the number of items. *)

val page_core_to_page : BookCore.PageCore.t -> page Lwt.t
(** Exposed for use in the book controller. *)
