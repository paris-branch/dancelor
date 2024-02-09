(** {1 Text formula converter}

    This module contains utilities to help convert text formulas into non-text
    formulas. *)

type 'p t
(** Abstract type of a converter from text formulas to formulas with predicates
    of type ['p]. *)

(** {2 Using} *)

val to_formula : 'p t -> TextFormulaType.t -> ('p Formula.t, string) Result.t
(** Convert a text formula to a formula using the given converter. *)

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

val map :
  (('p Formula.t -> 'q) * ('q -> 'p Formula.t option)) ->
  'p t ->
  'q t
(** Map over a converter given a function. *)

val merge : 'p t -> 'p t -> 'p t
(** Merge two converters together. Predicates that exist on both sides must have
    the same arity. The result is then the disjunction of the two formulas.
    Raises {!Invalid_arg} if a predicate exists in both with different
    arities. *)

val merge_l : 'p t list -> 'p t
(** Same as {!merge} on a non-empy list. *)
