(** {1 Lists} *)

(** {2 Standard Library}

    This module contains everything defined for lists by the OCaml standard
    library. For these functions, refer to the official documentation. *)

include module type of Stdlib.List

(** {2 Additional Contents} *)

(** {3 Sort Functions} *)

val sort_count : ('a -> 'a -> int) -> 'a list -> ('a * int) t
(** Same as {!sort_uniq}, but count duplicates. *)

(** {3 Others} *)

val hd_opt : 'a list -> 'a option

val hd_tl : 'a list -> 'a * 'a list
(** Pair of {!hd} and {!tl}.

    @raise Failure if the list is empty. *)

val remove : int -> 'a list -> 'a list
(** Remove the element at the given indice in the list. *)

val swap : int -> int -> 'a list -> 'a list
(** Swap elements at the given indices in the list. *)

(** {4 Contexts} *)

type 'a context = {
  element: 'a;
  index: int;
  previous: 'a option;
  next: 'a option;
  total: int;
}
(** The context of an element *)

val find_context : ('a -> bool) -> 'a t -> 'a context option
(** Finds the given element and return it and its context. *)

val findi_context : (int -> 'a -> bool) -> 'a t -> 'a context option
(** Same as {!find_context} but also provides the index in the test. *)

val map_context : ('a -> 'b) -> 'a context -> 'b context
(** Maps the given function on all the elements of the context. *)

(** {4 Bodies and feet} *)

val ft_opt : 'a list -> 'a option

val ft : 'a t -> 'a
(** Return the foot of the given list, that is the last element of the list.
    @raise Failure if the list is empty. *)

val bd_ft : 'a t -> 'a list * 'a
(** Return the pair of the body and the foot of the given list, that is the list
    without its last element and the last element.
    @raise Failure if the list is empty. *)

val intersperse : ?last: 'a -> 'a -> 'a t -> 'a t
(** [intersperse ?last x l] is the list [l] with [x] “interspersed” between all
    the elements of [l]. If [?last] is not [None], then the last occurence is
    not [x] but [last]. For instance, [intersperse ?last:" & " "," \["a"; "b";
    "c"; "d"\] = \["a"; ", "; "b"; ", "; "c"; " & "; "d"\]]. *)

val interspersei : ?last: (int -> 'a) -> (int -> 'a) -> 'a t -> 'a t
(** Same as {!intersperse} but with the indice passed as argument. *)

val singleton : 'a -> 'a list
(** [singleton x] is [[x]]. *)

val all_some : 'a option list -> 'a list option
(** Return [Some] if all the elements of the list are of the form [Some x], or
    [None] if at least one element is [None]. *)

(** {3 Association lists} *)

val extract_assoc_opt : 'a -> ('a * 'b) list -> ('b * ('a * 'b) list) option
(** Same as {!extract_assoc} but returns [None] instead of raising
    [Not_found]. *)

val map_first_some : ('a -> 'b option) -> 'a list -> 'b option
(** [map_first_some f l] maps [f] over [l] but stops the very first time that
    [f] returns [Some], and returns this value. *)

val to_option : ?more: ('a list -> 'a option) -> 'a list -> 'a option
(** Converts a lists to an option. The function [?more] is used for when the
    list is neither empty nor a singleton. It defaults to raising
    {!Invalid_arg}. *)
