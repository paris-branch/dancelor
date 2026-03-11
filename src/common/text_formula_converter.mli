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
  (('q Formula.t -> 'p) * ('p -> 'q Formula.t option)) ->
  'q t ->
  'p lifter

val make :
  debug_name: string ->
  debug_print: (Format.formatter -> 'p -> unit) ->
  raw: (string -> ('p Formula.t, string) result) ->
  ?lifters: 'p lifter list ->
  'p case list ->
  'p t
(** Make a converter from a list of {!type-case}s. The [~raw] argument is the
    case for standalone strings. For instance, in the formula ["bonjour
    baguette:oui monsieur :chocolat"], the [~raw] function will be applied to
    ["bonjour"] and ["monsieur"]. *)

(** {2 Debug} *)

val debug_name : 'p t -> string
(** Get the debug name of a converter. *)

val debug_print : 'p t -> Format.formatter -> 'p -> unit
(** Get the debug printer of a converter. *)
