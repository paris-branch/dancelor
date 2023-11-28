(** {1 Cache} *)

type ('k, 'v) t

val create : unit -> ('k, 'v) t

val use : cache:('k, 'v) t -> key:'k -> (unit -> 'v) -> 'v

val remove : cache:('k, 'v) t -> key:'k -> unit
