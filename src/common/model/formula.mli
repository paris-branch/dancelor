(** {1 Formula} *)

(** {2 Type} *)

type 'p t =
  | False
  | True
  | Not of 'p t
  | And of 'p t * 'p t
  | Or  of 'p t * 'p t
  | Pred of 'p
[@@deriving yojson]
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

(** {3 Miscellaneous} *)

val convert_opt : ('p -> 'q t option) -> 'p t -> 'q t option
(** Convert a formula over predicates of type ['p] into a formula over
    predicates of type ['q] given a function to convert predicates of type ['p]
    into predicates of type ['q]. The given function can fail in {!Option}. *)

val convert_res : ('p -> ('q t, 'e) Result.t) -> 'p t -> ('q t, 'e) Result.t
(** Same as {!convert_opt} except the given function can fail in {!Result}. *)

val pp : (Format.formatter -> 'p -> unit) -> Format.formatter -> 'p t -> unit
(** For debugging purposes. This is compatible with [ppx_deriving_show] but is a
    more usual representation of formulas. *)

val gen : 'p QCheck.Gen.t -> 'p t QCheck.Gen.t
(** For testing purposes: QCheck generator of formulas given a QCheck generator
    of predicates. *)

module Make_Serialisable :
  functor (M : Madge_common.SERIALISABLE) -> Madge_common.SERIALISABLE
  with type t = M.t t
