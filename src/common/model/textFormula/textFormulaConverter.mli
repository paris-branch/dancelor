(** {1 Text formula converter}

    This module contains utilities to help convert text formulas into non-text
    formulas. *)

type 'p t
(** Abstract type of a converter from text formulas to formulas with predicates
    of type ['p]. *)

(** {2 Using} *)

val to_formula : 'p t -> TextFormulaType.t -> ('p Formula.t, string) Result.t
(** Convert a text formula to a formula using the given converter. *)

val of_formula : 'p t -> 'p Formula.t -> TextFormulaType.t
(** Convert a formula to a text formula using the given converter. *)

(** {2 Case}

    Conversion cases. The full conversion consists in many tiny cases that can
    be built and combined separately. *)

type 'p case
(** Abstract type of one case. *)

val raw :
  (string -> ('p Formula.t, string) Result.t) ->
  'p case
(** Make a case for standalone strings. For instance, in the formula ["bonjour
    baguette:oui monsieur :chocolat"], the {!raw} case will apply to ["bonjour"]
    and ["monsieur"]. *)

val nullary : name:string -> 'p -> 'p case
(** Make a case for a nullary predicate of the given name converting to the
    given predicate. For instance, in the formula ["bonjour baguette:oui
    monsieur :chocolat"], a [nullary ~name:"chocolat" p] case will apply to
    [":chocolat"] while a [nullary ~name:"banana" q] case will not apply to
    anything. *)

val unary_string :
  name: string ->
  ((string -> 'p) * ('p -> string option)) ->
  'p case
(** Make a case whose argument must be a string. *)

val unary_int :
  name: string ->
  ((int -> 'p) * ('p -> int option)) ->
  'p case
(** Make a case whose argument must be an int. *)

val unary_raw :
  name: string ->
  cast: ((string -> 'i option) * ('i -> string)) ->
  type_: string ->
  (('i -> 'p) * ('p -> 'i option)) ->
  'p case
(** Make a case whose argument must be raw. The raw string is then passed to
    [~cast] to convert it to an ['i]ntermediary value. If it fails, an error
    message is created using [~type_]. For instance, {!unary_int} is [unary_raw
    ~cast:int_of_string_opt ~type_:"int"]. *)

val unary_lift :
  name: string ->
  converter: 'i t ->
  (('i Formula.t -> 'p) * ('p -> 'i Formula.t option)) ->
  'p case
(** Make a case that lifts other formulas. The argument is converted using the
    [converter] and the result is passed to the given lifting function. *)

(** {2 Building} *)

val make : 'p case list -> 'p t
(** Make a converter from a list of {!case}s. *)

val map : ('p Formula.t -> 'q) -> 'p t -> 'q t
(** Map over a converter given a function. This allows to lift formulas on ['p]
    to formulas on ['q] without a constructor. However, there is no way back. It
    is common to have both a [unary_lift ~name ~converter (constr, destr)] and a
    [map constr converter], the unary lift allowing to convert back to text. *)

type tiebreaker = Left | Right | Both

val merge : ?tiebreaker:tiebreaker -> 'p t -> 'p t -> 'p t
(** Merge two converters together. When predicates exist on both sides,
    [~tiebreaker] is used to choose which one to keep. If it is [Both] (the
    default), then the result is the disjunction of the two formulas. *)

val merge_l : 'p t list -> 'p t
(** Folds {!merge} on a non-empty list with the default tiebreaker. *)
