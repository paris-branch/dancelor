(** {1 Cache} *)

type ('key, 'value) t
(** Type of a cache binding ['key]s to ['value]s. *)

val create : ?lifetime:int -> unit -> ('key, 'value) t
(** Create a cache. The [?lifetime] argument allows to specify (in seconds) when
    bindings from the cache should be invalidated. Entries are _actually_
    removed from the cache during normal {!use} or when explicitly calling for a
    {!cleanup}. *)

val use : cache:('key, 'value) t -> key:'key -> (unit -> 'value) -> 'value
(** Looks up the [~key] in the [~cache]. If it exists, return its value.
    Otherwise, call the given function, store its result and return it. *)

val remove : cache:('key, 'value) t -> key:'key -> unit
(** Remove a [~key] from the [~cache]. *)

val cleanup : cache:('key, 'value) t -> unit
(** Explicitly call for a cache clean-up, removing entries that have passed
    their lifetime. *)
