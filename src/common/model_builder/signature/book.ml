module type S = sig
  (** {1 Book Signature}

      This module contains the signature of books, shared by both Dancelor's client
      and server. On the server side, some of these functions involve database
      accesses; on the client side, network calls. *)

  open Nes

  (** {2 Types} *)

  type page_dance = Core.Book.page_dance =
    | Dance_only
    | Dance_versions of (Core.Version.t Entry.id * Core.Version_parameters.t) NEList.t
    | Dance_set of Core.Set.t Entry.id * Core.Set_parameters.t
  [@@deriving variants]

  type page = Core.Book.page =
    | Part of NEString.t
    | Dance of Core.Dance.t Entry.id * page_dance
    | Versions of (Core.Version.t Entry.id * Core.Version_parameters.t) NEList.t
    | Set of Core.Set.t Entry.id * Core.Set_parameters.t
  [@@deriving variants]
  (** The type of one page in a book. A page either consists of a version (eg.
      in a book of tunes), or a set (eg. in a dance program). *)

  (** The type of a book. *)

  type t = Core.Book.t
  (** The type of a book. Even if it is known that it is a record, it should never
      be manipulated explicitly. *)

  type access = Entry.Access.Private.t [@@deriving yojson]
  type entry = t Entry.private_

  val make :
    name: NEString.t ->
    authors: Core.Person.t Entry.id list ->
    date: PartialDate.t option ->
    contents: page list ->
    remark: NEString.t option ->
    sources: Core.Source.t Entry.id list ->
    scddb_id: int option ->
    unit ->
    t

  (** {2 Field Getters} *)

  val name : t -> NEString.t
  val name' : entry -> NEString.t

  val authors : t -> Core.Person.t Entry.id list
  val authors' : entry -> Core.Person.t Entry.id list

  val date : t -> PartialDate.t option
  val date' : entry -> PartialDate.t option

  val contents : t -> page list
  val contents' : entry -> page list

  val remark : t -> NEString.t option
  val remark' : entry -> NEString.t option

  val sources : t -> Core.Source.t Entry.id list
  val sources' : entry -> Core.Source.t Entry.id list

  val scddb_id : t -> int option
  val scddb_id' : entry -> int option

  val equal : t -> t -> bool
  (** Structural equality. This is different from entry equality. *)

  (** {2 Advanced Field Getters} *)

  val slug : t -> NesSlug.t
  val slug' : entry -> NesSlug.t

  (** {2 Utilities} *)

  val contains_set : Core.Set.t Entry.Id.t -> entry -> bool

  val set_contents : page list -> t -> t

  (** {2 Warnings} *)

  type warning = Core.Book.warning =
    | Empty
    | Duplicate_set of Core.Set.t Entry.id
    | Duplicate_tune of Core.Tune.t Entry.id * (Core.Set.t Entry.id option * int) list
    | Set_dance_kind_mismatch of Core.Set.t Entry.id * Core.Dance.t Entry.id
  (* FIXME: a more specific type for (Set.t option * int) list. Maybe
     “occurrences”? And maybe with a record so that this “int” has a name? *)

  type warnings = warning list

  val warnings : entry -> warnings Lwt.t

  (** {2 Magic getter} *)

  (** Magic getter. On the client side, this hides an API call, which goes
      through the permissions mechanism. On the server side, this hides a call
      to the database. *)
  val get : t Entry.Id.t -> entry option Lwt.t
end
