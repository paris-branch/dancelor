open Nes

(** {2 Module and type aliases} *)

module Access = Access
module Id = Id
module Meta = Meta
module User = User

type 'value id = 'value Id.t
type user = User.t
type user_id = user id

(** {2 Entry} *)

type 'value t [@@deriving show]
(** A database entry.*)

(** {2 Builders} *)

val make :
  id: 'value Id.t ->
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  owner: User.t Id.t ->
  'value ->
  'value t

val make' :
  id: 'value Id.t ->
  ?meta: Meta.t ->
  access: Access.t ->
  'value ->
  'value t

(** {2 Getters} *)

val id : 'value t -> 'value Id.t
val id' : 'value t -> 'value t Id.t
val id_as_string : 'value t -> string
val value : 'value t -> 'value
val meta : 'value t -> Meta.t
val access : 'value t -> Access.t

(** {2 Comparison} *)

val equal : ('value -> 'value -> bool) -> 'value t -> 'value t -> bool
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!equal'}. *)

val equal' : 'value t -> 'value t -> bool

val compare : ('value -> 'value -> int) -> 'value t -> 'value t -> int
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!compare'}. *)

val compare' : 'value t -> 'value t -> int

(** {2 Serialisation} *)

val to_yojson : ('value -> Yojson.Safe.t) -> 'value t -> Yojson.Safe.t
val of_yojson : (Yojson.Safe.t -> ('value, string) result) -> Yojson.Safe.t -> ('value t, string) result

val to_yojson_no_id : ('value -> Yojson.Safe.t) -> 'value t -> Yojson.Safe.t
(** Variant of {!to_yojson} that doesn't serialise the id, for use in the
    database. *)

val of_yojson_no_id : 'value Id.t -> (Yojson.Safe.t -> ('value, string) result) -> Yojson.Safe.t -> ('value t, string) result
(** Variant of {!of_yojson} that expects the id as an argument instead of in
    the serialised form, for use in the database. *)

module J : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t t

(** {2 Advanced use} *)

val unsafe_set_value : 'value t -> 'new_value -> 'new_value t
(** Create an entry with the same id and metadata but holding a different
    value. *)
