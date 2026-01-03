(** {1 Entry} *)

(** {2 Module and type aliases} *)

module Access = Access
module Id = Id
module Meta = Meta
module User = User

type 'value id = 'value Id.t [@@deriving eq, show, yojson]

type user = User.t
type user_id = user id

(** {2 Entry} *)

type ('value, 'access) t [@@deriving eq, ord, show, yojson]
(** A generic database entry. It carries a value, and the access type is to be
    instantiated. *)

type 'value public = ('value, Access.public) t [@@deriving eq, ord, show, yojson]
type 'value private_ = ('value, Access.Private.t) t [@@deriving eq, ord, show, yojson]

val make :
  id: 'value Id.t ->
  ?meta: Meta.t ->
  access: 'access ->
  'value ->
  ('value, 'access) t

(** {2 Getters} *)

val id : ('value, 'access) t -> 'value Id.t
val id' : ('value, 'access) t -> ('value, 'access) t Id.t (* FIXME: why do we ever use this? *)
val id_as_string : ('value, 'access) t -> string

val value : ('value, 'access) t -> 'value
(** Return the value of an entry. *)

val value_public : 'value public -> 'value
(** Alias of {!value} with a more restrictive type. *)

val value_private_ : 'value private_ -> 'value
(** Alias of {!value} with a more restrictive type. *)

val meta : ('value, 'access) t -> Meta.t
val access : ('value, 'access) t -> 'access

(** {2 Comparison} *)

val equal' : ('value, 'access) t -> ('value, 'access) t -> bool
(** Entry equality is only based on ids; the value {!equal}, compatible with
    [\[@@deriving eq\]] therefore ignores its arguments. For manual use,
    {!equal'} is recommended. *)

val compare' : ('value, 'access) t -> ('value, 'access) t -> int
(** Entry comparison is only based on ids; the value {!comparison}, compatible
    with [\[@@deriving ord\]] therefore ignores its arguments. For manual use,
    {!compare'} is recommended. *)

(** {2 Serialisation} *)

val to_yojson_no_id : ('value -> Yojson.Safe.t) -> ('access -> Yojson.Safe.t) -> ('value, 'access) t -> Yojson.Safe.t
(** Variant of {!to_yojson} that doesn't serialise the id, for use in the
    database. *)

val of_yojson_no_id : 'value Id.t -> (Yojson.Safe.t -> ('value, string) result) -> (Yojson.Safe.t -> ('access, string) result) -> Yojson.Safe.t -> (('value, 'access) t, string) result
(** Variant of {!of_yojson} that expects the id as an argument instead of in
    the serialised form, for use in the database. *)

module JPublic : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t public

module JPrivate : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t private_

(** {2 Advanced use} *)

val unsafe_erase_value : ('value, 'access) t -> (unit, 'access) t
(** Create an entry with the same id and metadata but holding no value. *)

val unsafe_erase_value_and_access : ('value, 'access) t -> (unit, unit) t
(** Create an entry with the same id and metadata but holding no value and with
    no access information. *)
