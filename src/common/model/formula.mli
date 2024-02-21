(** {1 Formula} *)

(** {2 Type} *)

type 'p t =
  | False
  | True
  | Not of 'p t
  | And of 'p t * 'p t
  | Or  of 'p t * 'p t
  | Pred of 'p
[@@deriving eq, yojson]
(** Type of formulas carrying predicates of type ['p]. *)

(** {3 Constructors} *)

val false_ : 'p t
val true_ : 'p t
val not_ : 'p t -> 'p t

val and_ : 'p t -> 'p t -> 'p t
(** Conjunction of two formulas. *)

val and_l : 'p t list -> 'p t
(** Conjunction of a whole list of formulas. The empty conjunct is [True]. *)

val or_ : 'p t -> 'p t -> 'p t
(** Disjunction of two formulas. *)

val or_l : 'p t list -> 'p t
(** Disjunction of a whole list of formulas. The empty disjunct is [False]. *)

val pred : 'p -> 'p t

(** {3 Interpretation}

    We give semantics to formulas by interpreting them as a [float] between [0.]
    and [1.], [0.] morally meaning rejection and [1.] acceptance. *)

val interpret_false : float
val interpret_true : float

val interpret_bool : bool -> float
(** {!interpret_true} or {!interpret_false} depending on the boolean. *)

val interpret_not : float -> float
val interpret_and : float -> float -> float
val interpret_and_l : float list -> float
val interpret_or : float -> float -> float
val interpret_or_l : float list -> float

val interpret_exists : ('x -> float Lwt.t) -> 'x list -> float Lwt.t

val interpret : 'p t -> ('p -> float Lwt.t) -> float Lwt.t
(** Interpret a whole formula given a function to interpret predicates. *)

(** {3 Destructors} *)

val unPred : 'p t -> 'p option

val conjuncts : 'p t -> 'p t list
(** Returns all the conjuncts of the given formula. [and_l (conjuncts f) = f].
    The returned list is never empty. *)

val disjuncts : 'p t -> 'p t list
(** Returns all the disjuncts of the given formula. [or_l (disjuncts f) = f].
    The returned list is never empty. *)

val unCnf : 'p t -> 'p list list option
(** Given a formula in conjunctive normal form (CNF), return said CNF. Otherwise
    return [None]. This function {i does not} compute the CNF. *)

(** {3 Miscellaneous} *)

val optimise :
  ?lift_and: ('p -> 'p -> 'p option) ->
  ?lift_or: ('p -> 'p -> 'p option) ->
  ('p -> 'p) ->
  'p t -> 'p t
(** Optimise a formula, for instance with rules such as [⊥ ∧ ... → ⊥], given a
    function to optimise predicates. Optionally, one can also give [?lift_and]
    and [?lift_or] functions that can be used to lift the
    conjunction/disjunction of predicates into the predicates themselves. For
    instance, on [F(A) ∨ F(B)], [optimise] will call [lift_or (F(A)) (F(B))]
    which could yield [F(A ∨ B)] if the underlying predicates support this.
    [lift_and] and [lift_or] should be compatible with associativity and
    commutativity. *)

val convert_opt : ('p -> 'q t option) -> 'p t -> 'q t option
(** Convert a formula over predicates of type ['p] into a formula over
    predicates of type ['q] given a function to convert predicates of type ['p]
    into predicates of type ['q]. The given function can fail in {!Option}. *)

val convert_res : ('p -> ('q t, 'e) Result.t) -> 'p t -> ('q t, 'e) Result.t
(** Same as {!convert_opt} except the given function can fail in {!Result}. *)

val pp : (Format.formatter -> 'p -> unit) -> Format.formatter -> 'p t -> unit
(** For debugging purposes. This is compatible with [ppx_deriving_show] but is a
    more usual representation of formulas. *)

module Make_Serialisable :
  functor (M : Madge_common.SERIALISABLE) -> Madge_common.SERIALISABLE
  with type t = M.t t
