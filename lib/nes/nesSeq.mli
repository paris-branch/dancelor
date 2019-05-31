include module type of Seq

val sub : int -> 'a t -> 'a t
(** [sub n s] is the sequence that starts like [s] but contains at
   most [n] elements. *)

val is_empty : 'a t -> bool

val hd_opt : 'a t -> 'a option
(** return the first element of the sequence if there is one *)
