val bind : 'a option Lwt.t -> ('a -> 'b option Lwt.t) -> 'b option Lwt.t
val bind' : 'a option -> ('a -> 'b option Lwt.t) -> 'b option Lwt.t
val map' : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t
