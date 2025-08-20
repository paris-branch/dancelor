module type S = sig
  (** {1 Book Signature}

      This module contains the signature of books, shared by both Dancelor's client
      and server. On the server side, some of these functions involve database
      accesses; on the client side, network calls. *)

  open Nes

  (** {2 Types} *)

  type page = Core.Book.page =
    | Version of Core.Version.t Entry.t * Core.VersionParameters.t
    | Set of Core.Set.t Entry.t * Core.SetParameters.t
  [@@deriving variants]
  (** The type of one page in a book. A page either consists of a version (eg.
      in a book of tunes), or a set (eg. in a dance program). *)

  (** The type of a book. *)

  type t = Core.Book.t
  (** The type of a book. Even if it is known that it is a record, it should never
      be manipulated explicitly. *)

  val make :
    title: string ->
    ?subtitle: string ->
    ?short_title: string ->
    ?authors: Core.Person.t Entry.t list ->
    ?date: PartialDate.t ->
    ?contents: page list ->
    ?source: bool ->
    ?remark: string ->
    ?scddb_id: int ->
    unit ->
    t

  (** {2 Field Getters} *)

  val title : t -> string
  val title' : t Entry.t -> string

  val subtitle : t -> string
  val subtitle' : t Entry.t -> string

  val short_title : t -> string
  val short_title' : t Entry.t -> string

  val authors : t -> Core.Person.t Entry.t list Lwt.t
  val authors' : t Entry.t -> Core.Person.t Entry.t list Lwt.t

  val date : t -> PartialDate.t option
  val date' : t Entry.t -> PartialDate.t option

  val contents : t -> page list Lwt.t
  val contents' : t Entry.t -> page list Lwt.t

  val source : t -> bool
  val source' : t Entry.t -> bool

  val remark : t -> string
  val remark' : t Entry.t -> string

  val scddb_id : t -> int option
  val scddb_id' : t Entry.t -> int option

  (** {2 Advanced Field Getters} *)

  val slug : t -> Entry.Slug.t
  val slug' : t Entry.t -> Entry.Slug.t

  (** {2 Utilities} *)

  val contains_set : Core.Set.t Entry.Id.t -> t Entry.t -> bool
  val compare : t Entry.t -> t Entry.t -> int
  val equal : t Entry.t -> t Entry.t -> bool

  val lilypond_contents_cache_key : t -> string Lwt.t
  val lilypond_contents_cache_key' : t Entry.t -> string Lwt.t

  (** {2 Warnings} *)

  type warning = Core.Book.warning =
    | Empty
    | DuplicateSet of Core.Set.t Entry.t
    | DuplicateVersion of Core.Tune.t Entry.t * (Core.Set.t Entry.t option * int) list
    | SetDanceMismatch of Core.Set.t Entry.t * Core.Dance.t Entry.t
  (* FIXME: a more specific type for (Set.t option * int) list. Maybe
     “occurrences”? And maybe with a record so that this “int” has a name? *)

  type warnings = warning list

  val warnings : t Entry.t -> warnings Lwt.t

  val page_core_to_page : Core.Book.Page.t -> page Lwt.t
  (** Exposed for use in the book controller. *)

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> t Entry.t option Lwt.t
end
