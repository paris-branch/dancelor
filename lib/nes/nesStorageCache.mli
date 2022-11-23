(** {1 Cache} *)

type ('key, 'value) t
(** Abstract type for caches from keys to values. Values are actually linked to
    some hashed version of the keys. *)

val create : unit -> ('key, 'value) t
(** Create a new cache. *)

val add: cache:('key, 'value) t -> hash:int -> value:'value -> unit
(** Add a value to the cache, given its hash. This is useful in particular when
    reconstructing a cache from somewhere else. *)

val use : cache:('key, 'value) t -> key:'key -> (int -> 'value) -> 'value
(** Use the cache. One gives a key and a function that builds a value given a
    hash. If they key is already known, then its associated value is used
    immediately. Otherwise, the function is called and the value is saved and
    returned. *)

val remove : cache:('key, 'value) t -> key:'key -> unit
(** Removes a binding from the cache. *)
