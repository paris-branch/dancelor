include module type of Seq

val sub : int -> 'a t -> 'a t
(** [sub n s] is the sequence that starts like [s] but contains at
   most [n] elements. *)

val is_empty : 'a t -> bool

val hd_opt : 'a t -> 'a option
(** return the first element of the sequence if there is one *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [a1; ...; an; ...]] checks if all elements of the (possibly
   infinite) sequence satisfy the predicate [p]. That is, it returns [(p a1) &&
   (p a2) && ... && (p an) && ...]. It returns [false] as soon as possible. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [a1; ...; an; ...]] checks if at least one element of the
   (possibly infinite) sequence satisfies the predicate [p]. That is, it returns
   [(p a1) || (p a2) || ... || (p an) || ...]. It returns [true] as soon as
   possible. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Same as {!NesSeq.for_all}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two sequences are determined to have
   different lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Same as {!NesSeq.exists}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two sequences are determined to have
   different lengths. *)

val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
