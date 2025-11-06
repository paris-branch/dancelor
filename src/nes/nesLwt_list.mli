include module type of Lwt_list

val sort_multiple : ('a -> 'a -> int Lwt.t) list -> 'a list -> 'a list Lwt.t
(** Same as {!Lwt_list.sort} except it takes several comparison functions. The
    function tries them one by one until one of them separates the two values. *)

val increasing : ('a -> 'b Lwt.t) -> ('b -> 'b -> int) -> ('a -> 'a -> int Lwt.t)
val decreasing : ('a -> 'b Lwt.t) -> ('b -> 'b -> int) -> ('a -> 'a -> int Lwt.t)

val compare_multiple : ('a -> 'a -> int Lwt.t) list -> ('a -> 'a -> int Lwt.t)
