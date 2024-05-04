(** {1 Depart — Artificial dependencies} *)

val depends : on:'a -> 'b -> unit
(** [depends ~on value] declares an artificial GC dependency from [value] on
    [on]. [value] will not be garbage collected until [on] is. [value] may of
    course outlive [on] if other other objects depend on it, artificially or
    not. *)

val keep_forever : 'a -> unit
(** [keep_forever value] declares an artificial GC dependency from [value] on
    the global root set. [value] will never be garbage collected. *)
