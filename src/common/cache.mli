type ('a, 'b) t

val create : unit -> ('a, 'b) t

val use : ('a, 'b) t -> 'a -> (unit -> 'b) -> 'b

val remove : ('a, 'b) t -> 'a -> unit
