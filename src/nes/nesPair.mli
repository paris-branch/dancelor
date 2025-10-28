val fst : 'a * 'b -> 'a
(** Get the first element of the pair. *)

val snd : 'a * 'b -> 'b
(** Get the second element of the pair. *)

val set_fst : 'a -> 'w * 'b -> 'a * 'b
(** Replace the first component of the pair. *)

val set_snd : 'b -> 'a * 'w -> 'a * 'b
(** Replace the second component of the pair. *)

val map_fst : ('a -> 'c) -> 'a * 'b -> 'c * 'b
(** Map the given function to the first component of the pair. *)

val map_fst_lwt : ('a -> 'c Lwt.t) -> 'a * 'b -> ('c * 'b) Lwt.t
(** Lwt version of [map_fst]. *)

val map_snd : ('b -> 'd) -> 'a * 'b -> 'a * 'd
(** Map the given function to the second component of the pair. *)

val map_snd_lwt : ('b -> 'd Lwt.t) -> 'a * 'b -> ('a * 'd) Lwt.t
(** Lwt version of [map_snd]. *)

val map : ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
(** Map the given functions on the arguments of the pair. *)

val map_lwt : ('a -> 'c Lwt.t) -> ('b -> 'd Lwt.t) -> 'a * 'b -> ('c * 'd) Lwt.t
(** Lwt version of [map]. *)

val map_both : ('a -> 'b) -> 'a * 'a -> 'b * 'b
(** Map the given function on both arguments of the pair. *)

val map_both_lwt : ('a -> 'b Lwt.t) -> 'a * 'a -> ('b * 'b) Lwt.t
(** Lwt version of [map_both]. *)

val map2 : ('a -> 'c -> 'e) -> ('b -> 'd -> 'f) -> 'a * 'b -> 'c * 'd -> 'e * 'f
(** Map the given functions on the arguments of the two pairs. [map2 f g
    (x1, y1) (x2, y2)] is [(f x1 x2, g y1 y2)]. *)

val cons : 'a -> 'b -> 'a * 'b
(** Construct a pair. This is meant to be partially applied, eg. [cons x] is the
    function that adds [x] in front of its argument. *)

val snoc : 'b -> 'a -> 'a * 'b
(** Construct a pair, from arguments in reverse order. This is meant to be
    partially applied, eg. [snoc y] is the function that adds [y] after its
    argument. *)
