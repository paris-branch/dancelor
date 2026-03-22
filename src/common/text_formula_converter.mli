(** {1 Text formula converter}

    This module contains utilities to help convert text formulas into non-text
    formulas. *)

type 'p t
(** Abstract type of a converter between text formulas and formulas with
    predicates of type ['p]. *)

(** {2 Using} *)

val raw : 'p t -> string -> ('p Formula.t, string) Result.t
(** Convert a “raw” string to a formula using the given converter. In the
    formula ["bonjour baguette:oui monsieur :chocolat"], raw strings would be
    ["bonjour"] and ["monsieur"]. *)

val text_formula_to_formula : 'p t -> Text_formula_type.t -> ('p Formula.t, string) Result.t
(** Convert a text formula to a formula using the given converter. *)

val formula_to_text_formula : 'p t -> 'p Formula.t -> Text_formula_type.t
(** Convert a formula to a text formula using the given converter. *)

val optimise : 'p t -> 'p Formula.t -> 'p Formula.t
(** Optimise a formula using the information contained in a converter. *)

(** {2 Case}

    Conversion cases. The full conversion consists in many tiny cases that can
    be built and combined separately. *)

type 'p case
(** Abstract type of one case. *)

val nullary : name: string -> 'p -> 'p case
(** Make a case for a nullary predicate of the given name converting to the
    given predicate. For instance, in the formula ["bonjour baguette:oui
    monsieur :chocolat"], a [nullary ~name:"chocolat" p] case will apply to
    [":chocolat"] while a [nullary ~name:"banana" q] case will not apply to
    anything. *)

type wrap_back =
  | Always
  | Never
  | Not_pred
  | Not_raw
  | Custom of (Text_formula_type.t -> Text_formula_type.t)

val unary_string :
  ?wrap_back: wrap_back ->
  name: string ->
  ((string -> 'p) * ('p -> string option)) ->
  'p case
(** Make a case whose argument must be a string.

    The optional argument [?wrap_back] ([Always] by default) specifies whether,
    when converting back to text formula, a unary text predicate should be used
    to wrap the value. It can be useful to set it to [NotPred] when the unary
    predicate is exactly the same as {!raw}. *)

val unary_int :
  ?wrap_back: wrap_back ->
  name: string ->
  ((int -> 'p) * ('p -> int option)) ->
  'p case
(** Make a case whose argument must be an int. See {!unary_string} for the
    [?wrap_back] argument. *)

val unary_id :
  ?wrap_back: wrap_back ->
  name: string ->
  (('a Entry.Id.t -> 'p) * ('p -> 'a Entry.Id.t option)) ->
  'p case
(** Make a case whose argument must be a ids. *)

val unary_raw :
  ?wrap_back: wrap_back ->
  name: string ->
  cast: ((string -> 'i option) * ('i -> string)) ->
  type_: string ->
  (('i -> 'p) * ('p -> 'i option)) ->
  'p case
(** Make a case whose argument must be raw. The raw string is then passed to
    [~cast] to convert it to an ['i]ntermediary value. If it fails, an error
    message is created using [~type_]. For instance, {!unary_int} is [unary_raw
    ~cast:int_of_string_opt ~type_:"int"]. See {!unary_string} for the
    [?wrap_back] argument. *)

(** {2 Building} *)

type 'a lifter

type inline = Inline | No_inline
(** An inline lifter makes the cases of the underlying converter accessible to
    the containing converter. *)

val lifter :
  name: string ->
  ?inline: inline ->
  ?down_not: ('q Formula.t -> 'p Formula.t option) ->
  ?down_or: ('q Formula.t -> 'q Formula.t -> 'p option) ->
  ?down_and: ('q Formula.t -> 'q Formula.t -> 'p option) ->
  (('q Formula.t -> 'p) * ('p -> 'q Formula.t option)) ->
  'q t ->
  'p lifter
(** [lifter ~name (lift, unlift) converter] creates a {!lifter} for sub-formulas
    given a [converter] for sub-formulas, a function to [lift] a sub-formula
    into a predicate, and a partial function to [unlift] a sub-predicate into a
    formula.

    The optional argument [?proper] indicates whether this lifter has no other
    semantic meaning. For instance, the list's [exists:] lifter is not proper
    but the entry's [value:] one is.

    The optional arguments [?down_or] and [?down_and] are used to push
    disjunctions and conjunctions down into the lifted sub-formulas. Lifters are
    assumed to have no other semantic meaning by default, which makes them
    distributed over/commute with conjunctions and disjunctions, but these
    arguments can be used to specify how to push conjunctions and disjunctions
    down into the lifted sub-formulas when this is not the case. For instance,
    the list's [exists:] lifter can push down disjunctions but not conjunctions,
    and therefore should have [~down_and: (fun _ _ -> None)]. It can however
    rely on the default value for [?down_or].

    The optional argument [?inline] controls whether the sub-predicates should
    be made available in the current level. For instance, an entry's value
    predicates should be inlined at the entry level, such that one may write
    [x:] instead of [value:x:]. *)

val make :
  debug_name: string ->
  debug_print: (Format.formatter -> 'p -> unit) ->
  raw: (string -> ('p Formula.t, string) result) ->
  ?lifters: 'p lifter list ->
  ?pre_optimise: ('p Formula.t -> 'p Formula.t) ->
  'p case list ->
  'p t
(** Make a converter from a list of {!type-case}s. The [~raw] argument is the
    case for standalone strings. For instance, in the formula ["bonjour
    baguette:oui monsieur :chocolat"], the [~raw] function will be applied to
    ["bonjour"] and ["monsieur"].

    [?pre_optimise] is an optional custom function that will be applied before
    optimising a formula. By default, it does nothing. *)

(** {2 Debug} *)

val debug_name : 'p t -> string
(** Get the debug name of a converter. *)

val debug_print : 'p t -> Format.formatter -> 'p -> unit
(** Get the debug printer of a converter. *)
