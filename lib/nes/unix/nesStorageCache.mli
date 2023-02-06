(** {1 Cache} *)

type ('key , 'value )t
(** Abstract type for caches from keys to values. Values are actually linked to
    some hashed version of the keys. *)

type hash
(** Abstract type for key hashes. *)

val hash_to_string : hash -> string
(** Returns a string representation of a hash. This is guaranteed to only
    contain alpha-numeric characters. *)

val pp_hash : Format.formatter -> hash -> unit
(** Prints the string representation of a hash to a formatter. *)

val hash_from_string : string -> hash
(** Returns a hash from its string representation.

    @raise Invalid_argument if the string does not represent a valid hash. *)

val create : unit -> ('key , 'value ) t
(** Create a new cache. *)

val add : cache: ('key , 'value ) t -> hash: hash -> value: 'value -> unit
(** Add a value to the cache, given its hash. This is useful in particular when
    reconstructing a cache from somewhere else. *)

val use : cache: ('key , 'value ) t -> key: 'key -> (hash -> 'value) -> 'value
(** Use the cache. One gives a key and a function that builds a value given a
    hash. If they key is already known, then its associated value is used
    immediately. Otherwise, the function is called and the value is saved and
    returned. *)

val remove : cache: ('key , 'value ) t -> key: 'key -> unit
(** Removes a binding from the cache. *)
