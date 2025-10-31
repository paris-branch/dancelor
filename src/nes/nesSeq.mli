include module type of Seq

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p [a1; ...; an; ...]] checks if all elements of the (possibly
    infinite) sequence satisfy the predicate [p]. That is, it returns [(p a1) &&
    (p a2) && ... && (p an) && ...]. It returns [false] as soon as possible. *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p [a1; ...; an; ...]] checks if at least one element of the
    (possibly infinite) sequence satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an) || ...]. It returns [true] as soon as
    possible. *)

val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
