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

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val title : t -> string Lwt.t
val subtitle : t -> string Lwt.t
val short_title : t -> string Lwt.t
val date : t -> PartialDate.t option Lwt.t
val contents : t -> page list Lwt.t
val source : t -> bool Lwt.t (* FIXME: Should be removed *)
val remark : t -> string Lwt.t
val scddb_id : t -> int option Lwt.t

(** {2 Advanced Field Getters} *)

val is_source : t -> bool Lwt.t

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
val equal : t -> t -> bool Lwt.t
val compare : t -> t -> int
(* FIXME: this comparison function is a hack; a cleaner (more consistent with
   the rest of Dancelor) version should check the slug and return a Lwt.t *)

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

(** {2 API Getters & Setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  title:string ->
  ?date:PartialDate.t ->
  ?contents_and_parameters:page list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  BookFilter.t ->
  t Score.t list Lwt.t

val update :
  ?status:Status.t ->
  slug:t Slug.t ->
  title:string ->
  ?date:PartialDate.t ->
  ?contents_and_parameters:page list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> unit Lwt.t
