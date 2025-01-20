open Nes

type 'a t [@@deriving yojson, show]
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
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!equal'}. *)

val equal' : 'a t -> 'a t -> bool

(** {2 Serialisation} *)

module J : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t t
