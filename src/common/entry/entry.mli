open Nes

module Id = Id
module Slug = ESlug

type 'a t [@@deriving show]
(** A database entry. Can either be a full database entry or a dummy. Dummies
    should be avoided, and the getters will fail when dealing with them, but can
    be used in places that require dealing with a database table. *)

val is_dummy : 'a t -> bool
(** Test whether the given entry is a dummy. *)

(** {2 Builders} *)

val make :
  id: 'a Id.t ->
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
  id: 'a Id.t ->
  ?meta: meta ->
  'a ->
  'a t

val make_dummy : 'a -> 'a t

(** {2 Getters} *)

exception UsedGetterOnDummy
(** Exception raised when trying to access field of a dummy entry. *)

val id : 'a t -> 'a Id.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val id' : 'a t -> 'a t Id.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val id_as_string : 'a t -> string
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

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!equal'}.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val equal' : 'a t -> 'a t -> bool
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!compare'}.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val compare' : 'a t -> 'a t -> int
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

(** {2 Serialisation} *)

val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
val of_yojson : (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result

val to_yojson' : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
(** Variant of {!to_yojson} that doesn't serialise the id.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val of_yojson' : 'a Id.t -> (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result
(** Variant of {!of_yojson} that expects the id as an argument instead of in
    the serialised form. *)

module J : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t t
