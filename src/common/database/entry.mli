open Nes

type 'a t [@@deriving show]
(** A database entry. Can either be a full database entry or a dummy. Dummies
    should be avoided, and the getters will fail when dealing with them, but can
    be used in places that require dealing with a database table. *)

val is_dummy : 'a t -> bool
(** Test whether the given entry is a dummy. *)

(** {2 Builders} *)

val make :
  slug: 'a Slug.t ->
  ?status: Status.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  'a ->
  'a t

type meta

val make_meta :
  ?status: Status.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  unit ->
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

val status : 'a t -> Status.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val created_at : 'a t -> Datetime.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val modified_at : 'a t -> Datetime.t
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

val value : 'a t -> 'a

(** {2 Comparison} *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!equal'}.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val equal' : 'a t -> 'a t -> bool
(** @raise UsedGetterOnDummy if the entry is a dummy. *)

(** {2 Serialisation} *)

val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
val of_yojson : (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result

val to_yojson' : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
(** Variant of {!to_yojson} that doesn't serialise the slug.

    @raise UsedGetterOnDummy if the entry is a dummy. *)

val of_yojson' : 'a Slug.t -> (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result
(** Variant of {!of_yojson} that expects the slug as an argument instead of in
    the serialised form. *)

module J : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t t
