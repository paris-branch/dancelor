open Nes

(** {2 Module and type aliases} *)

module Access = Access
module Id = Id
module Meta = Meta
module User = User

type 'a id = 'a Id.t
type user = User.t
type user_id = user id

(** {2 Entry} *)

type 'a t [@@deriving show]
(** A database entry.*)

(** {2 Builders} *)

val make :
  id: 'a Id.t ->
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  owner: User.t Id.t ->
  'a ->
  'a t

val make' :
  id: 'a Id.t ->
  ?meta: Meta.t ->
  access: Access.t ->
  'a ->
  'a t

(** {2 Getters} *)

val id : 'a t -> 'a Id.t
val id' : 'a t -> 'a t Id.t
val id_as_string : 'a t -> string
val value : 'a t -> 'a
val meta : 'a t -> Meta.t
val access : 'a t -> Access.t

(** {2 Comparison} *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!equal'}. *)

val equal' : 'a t -> 'a t -> bool

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Comparison for ppx_deriving_yojson. The first argument is ignored. Use {!compare'}. *)

val compare' : 'a t -> 'a t -> int

(** {2 Serialisation} *)

val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
val of_yojson : (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result

val to_yojson' : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t
(** Variant of {!to_yojson} that doesn't serialise the id. *)

val of_yojson' : 'a Id.t -> (Yojson.Safe.t -> ('a, string) result) -> Yojson.Safe.t -> ('a t, string) result
(** Variant of {!of_yojson} that expects the id as an argument instead of in
    the serialised form. *)

module J : functor (M : Madge.JSONABLE) ->
  Madge.JSONABLE with
  type t = M.t t

(** {2 Advanced use} *)

val unsafe_set_value : 'a t -> 'b -> 'b t
(** Create an entry with the same id and metadata but holding a different
    value. *)
