(** {1 Options} *)

(** {2 Standard Library}

    This module contains everything defined for lists by the OCaml standard
    library. For these functions, refer to the official documentation. *)

include module type of Stdlib.Option

(** {2 Additional Contents} *)

val choose : tie: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
(** [choose ~tie first second] returns [Some x] when one of [first] or [second]
    is [Some x] and the other is bottom. If they are both [None], [None] is
    returned. If they are both [Some], [tie] is called. *)

val second : 'a -> 'a -> 'a
(** For {!NesOption.choose}'s [tie] argument. Takes the second argument. *)

val return : 'a -> 'a option
(** Same as {!some}. *)

val of_string_nonempty : string -> string option
(** Maps [""] to [None] and other strings to [Some]. *)

val fold' : none: (unit -> 'a) -> some: ('b -> 'a) -> 'b option -> 'a
(** Variant of {!fold} where [~none] is a thunk. *)

val value' : default: (unit -> 'a) -> 'a option -> 'a
(** Variant of {!default} where [~default] is a thunk. *)
