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

val hd_tl : 'a list -> 'a * 'a list
(** Pair of {!hd} and {!tl}. @raise Failure if the list is empty. *)

val remove : int -> 'a list -> 'a list
(** Remove the element at the given indice in the list. *)

val replace : int -> 'a -> 'a list -> 'a * 'a list
(** Replace the element at the given indice by the given element; return the
    element previously at the given indice and the new list. *)

val swap : int -> int -> 'a list -> 'a list
(** Swap elements at the given indices in the list. *)

(** {4 Contexts} *)

type 'a context = {
  element : 'a;
  index : int;
  previous : 'a option;
  next : 'a option;
  total : int;
}
(** The context of an element *)

val find_context : ('a -> bool) -> 'a t -> 'a context option
(** Finds the given element and return it and its context. *)

val findi_context : (int -> 'a -> bool) -> 'a t -> 'a context option
(** Same as {!find_context} but also provides the index in the test. *)

val map_context : ('a -> 'b) -> 'a context -> 'b context
(** Maps the given function on all the elements of the context. *)

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

val intersperse : ?last:'a -> 'a -> 'a t -> 'a t
(** [intersperse ?last x l] is the list [l] with [x] “interspersed” between all
    the elements of [l]. If [?last] is not [None], then the last occurence is
    not [x] but [last]. For instance, [intersperse ?last:" & " "," \["a"; "b";
    "c"\] = \["a"; ", "; "b"; " & "; "c"\]]. *)

val interspersei : ?last:(int -> 'a) -> (int -> 'a) -> 'a t -> 'a t
(** Same as {!intersperse} but with the indice passed as argument. *)

val compare_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> 'a t -> int Lwt.t
(** Same as {!compare} when the comparison function returns an Lwt value. *)

val singleton : 'a -> 'a list
(** [singleton x] is [[x]]. *)

val snoc : 'a list -> 'a -> 'a list
(** Append at the end. [snoc l x] is [l @ \[x\]]*)

val all_some : 'a option list -> 'a list option
(** Return [Some] if all the elements of the list are of the form [Some x], or
    [None] if at least one element is [None]. *)

val apply : ('a -> 'b) list -> 'a -> 'b list
(** [apply \[f1; f2; f3\] x = \[f1 x; f2 x; f3 x\]]. *)
