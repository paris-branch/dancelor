open Nes

type 'a t [@@deriving eq, ord, show, yojson]
(** A database entry. Can either be a full database entry or a dummy. Dummies
    should be avoided, and the getters will fail when dealing with them, but can
    be used in places that require dealing with a database table. *)

val is_dummy : 'a t -> bool
(** Test whether the given entry is a dummy. *)

(** {2 Builders} *)

val make :
  slug: 'a Slug.t ->
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  'a ->
  'a t

type meta

val make_meta :
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  unit ->
  meta

val update_meta :
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  meta ->
  meta

val make' :
  slug: 'a Slug.t ->
  ?meta: meta ->
  'a ->
  'a t

val make_dummy : 'a -> 'a t

(** {2 Getters} *)

exception UsedGetterOnDummy
(** Exception raised when trying to access field of a dummy entry. *)

val slug : 'a t -> 'a Slug.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val slug' : 'a t -> 'a t Slug.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val slug_as_string : 'a t -> string
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val value : 'a t -> 'a

val meta : 'a t -> meta
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val status : meta -> Status.t
val status' : 'a t -> Status.t

val privacy : meta -> Privacy.t
val privacy' : 'a t -> Privacy.t

val created_at : meta -> Datetime.t

val modified_at : meta -> Datetime.t

(** {2 Comparison} *)

val equal' : 'a t -> 'a t -> bool
(** Variant of {!equal} that doesn't require a slug equality.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val compare' : 'a t -> 'a t -> int
(** Variant of {!compare} that doesn't require a slug comparison.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

(** {2 Serialisation} *)

val yojson_of_t' : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
(** Variant of {!yojson_of_t} that doesn't serialise the slug.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val t_of_yojson' : 'a Slug.t -> (Yojson.Safe.t -> 'a) -> Yojson.Safe.t -> 'a t
(** Variant of {!t_of_yojson} that expects the slug as an argument instead of in
    the serialised form. *)

module J : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t t
