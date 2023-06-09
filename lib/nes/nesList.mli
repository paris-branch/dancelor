(** {1 Lists} *)

(** {2 Standard Library}

    This module contains everything defined for lists by the OCaml standard
    library. For these functions, refer to the official documentation. *)

include module type of Stdlib.List

(** {2 Additional Contents} *)

(** {3 Sort Functions} *)

val sort_count : ('a -> 'a -> int) -> 'a list -> ('a * int) t
(** Same as {!sort_uniq}, but count duplicates. *)

val sort_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> 'a t Lwt.t
(** Same as {!sort} when the comparison returns an Lwt value. *)

val sort_uniq_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> 'a t Lwt.t
(** Same as {!sort_uniq} when the comparison returns an Lwt value. *)

val sort_count_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> ('a * int) t Lwt.t
(** Same as {!sort_count} when the comparison returns an Lwt value. *)

val merge_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> 'a t -> 'a t Lwt.t
(** Same as {!merge} when the comparison returns an Lwt value. *)

(** {3 Others} *)

val map_filter : ('a -> 'b option) -> 'a t -> 'b t
(** Legacy alias for {!filter_map}. *)
(* FIXME: get rid of this function and add constraint on OCaml's version. *)

val sub : int -> 'a t -> 'a t

val hd_opt : 'a t -> 'a option
(** Return the first element of the given list or [None] if the list is empty. *)

(** {4 Bodies and feet} *)

val bd : 'a t -> 'a list
(** Return the body of the given list, that is the list without its last
    element.
    @raise Failure if the list is empty. *)

val bd_opt : 'a t -> 'a list option
(** Return the body of the given list, that is the list without its last
    element, or [None] if the list is empty. *)

val ft : 'a t -> 'a
(** Return the foot of the given list, that is the last element of the list.
    @raise Failure if the list is empty. *)

val ft_opt : 'a t -> 'a option
(** Return the foot of the given list, that is the last element of the list, or
    [None] if the list is empty. *)

val bd_ft : 'a t -> 'a list * 'a
(** Return the pair of the body and the foot of the given list, that is the list
    without its last element and the last element.
    @raise Failure if the list is empty. *)

val bd_ft_opt : 'a t -> ('a list * 'a) option
(** Return the pair of the body and the foot of the given list, that is the list
    without its last element and the last element, or [None] if the list is
    empty.
    @raise Failure if the list is empty. *)

val intertwine : (int -> 'a) -> 'a t -> 'a t

val compare_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> 'a t -> int Lwt.t
(** Same as {!compare} when the comparison function returns an Lwt value. *)

val singleton : 'a -> 'a list
(** [singleton x] is [[x]]. *)
