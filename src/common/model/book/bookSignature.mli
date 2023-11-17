(** {1 Book Signature}

    This module contains the signature of books, shared by both Dancelor's client
    and server. On the server side, some of these functions involve database
    accesses; on the client side, network calls. *)

open Nes

(** {2 Types} *)

type page = BookCore.page =
  | Version   of VersionCore.t * VersionParameters.t
  | Set       of     SetCore.t *     SetParameters.t
  | InlineSet of     SetCore.t *     SetParameters.t
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

val slug        : t -> t Slug.t
val status      : t -> Status.t
val title       : t -> string
val subtitle    : t -> string
val short_title : t -> string
val date        : t -> PartialDate.t option
val contents    : t -> page list Lwt.t
val source      : t -> bool
val remark      : t -> string
val scddb_id    : t -> int option
val modified_at : t -> Datetime.t
val created_at  : t -> Datetime.t

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
  type t = BookCore.Filter.t

  val accepts : t -> BookCore.t -> float Lwt.t

  val isSource : t
  val memSet : SetCore.t -> t
  val memTuneDeep : TuneCore.t -> t
  val memVersionDeep : VersionCore.t -> t

  val raw : string -> t TextFormula.or_error
  val nullary_text_predicates : (string * t) list
  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
  val from_string : ?filename:string -> string -> t TextFormula.or_error
end

(** {2 API Getters & Setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  title:string ->
  ?date:PartialDate.t ->
  ?contents:page list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val make :
  ?status:Status.t ->
  slug:t Slug.t ->
  title:string ->
  ?date:PartialDate.t ->
  ?contents:page list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t
(** Low-level unsafe book creation. Prefer {!make_and_save} or {!update}. *)

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t

val update :
  ?status:Status.t ->
  slug:t Slug.t ->
  title:string ->
  ?date:PartialDate.t ->
  ?contents:page list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> unit Lwt.t
