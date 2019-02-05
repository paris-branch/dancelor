include module type of Seq

val sub : int -> 'a t -> 'a t
(** [sub n s] is the sequence that starts like [s] but contains at
   most [n] elements. *)
