include module type of Lwt_list

val sort : ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t
(** Same as {!Lwt_list.sort} except for a Lwt comparison. *)

val stable_sort : ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t
(** Same as {!Lwt_list.stable_sort} except for a Lwt comparison. *)

val fast_sort : ('a -> 'a -> int Lwt.t) -> 'a list -> 'a list Lwt.t
(** Same as {!Lwt_list.fast_sort} except for a Lwt comparison. *)

val sort_multiple : ('a -> 'a -> int Lwt.t) list -> 'a list -> 'a list Lwt.t
(** Same as {!Lwt_list.sort} except it takes several comparison functions. The
    function tries them one by one until one of them separates the two values. *)

val increasing : ('a -> 'b Lwt.t) -> ('b -> 'b -> int) -> ('a -> 'a -> int Lwt.t)
val decreasing : ('a -> 'b Lwt.t) -> ('b -> 'b -> int) -> ('a -> 'a -> int Lwt.t)

val compare_multiple : ('a -> 'a -> int Lwt.t) list -> ('a -> 'a -> int Lwt.t)

val find_mapi_s : (int -> 'a -> 'b option Lwt.t) -> 'a list -> 'b option Lwt.t
