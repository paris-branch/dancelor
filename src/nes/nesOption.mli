(** {1 Lists} *)

(** {2 Standard Library}

    This module contains everything defined for lists by the OCaml standard
    library. For these functions, refer to the official documentation. *)

include module type of Stdlib.Option

(** {2 Additional Contents} *)

val compare_lwt : ('a -> 'a -> int Lwt.t) -> 'a t -> 'a t -> int Lwt.t
(** Same as {!compare} when the comparison function returns an Lwt value. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
val compose : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
val map : ('a -> 'b) -> ('a t -> 'b t)

val unwrap : 'a t -> 'a
val unwrap_or : default:'a -> 'a t -> 'a

val unwrap_map_or : default:'b -> ('a -> 'b) -> 'a t -> 'b

val ifsome : ('a -> unit) -> ('a t -> unit)
val ifsome_lwt : ('a -> unit Lwt.t) -> ('a t -> unit Lwt.t)

val choose : tie:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
(** [choose ~tie first second] returns [Some x] when one of [first] or [second]
    is [Some x] and the other is bottom. If they are both [None], [None] is
    returned. If they are both [Some], [tie] is called. *)

val fail : 'a -> 'a -> 'a
(** For {!NesOption.choose}'s [tie] argument. Fails. *)

val second : 'a -> 'a -> 'a
(** For {!NesOption.choose}'s [tie] argument. Takes the second argument. *)

val assert_some : 'a t -> 'a t
(** Fails when = None *)

val assert_ : bool -> unit t
(** [None] if [false], [Some ()] if [true]. *)

val concat : ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
(** Concatenation of the monoid given the concatenation of the embedded one. *)

val concat_l : ('a -> 'a -> 'a) -> 'a option list -> 'a option
(** Same as {!concat} but for a list of options. *)
