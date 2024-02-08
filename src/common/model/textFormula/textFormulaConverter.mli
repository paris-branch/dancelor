(** {1 Text formula converter}

    This module contains utilities to help convert text formulas into non-text
    formulas. *)

type 'p t
(** Abstract type of a converter from text formulas to formulas with predicates
    of type ['p]. *)

(** {2 Using} *)

val to_formula : 'p t -> TextFormulaType.t -> ('p Formula.t, string) Result.t
(** Convert a text formula to a formula using the given converter. *)

(** {2 Predicate bindings} *)

type 'p predicate_binding
(** Abstract type of a binding in the map of predicates. *)

val nullary : name:string -> 'p -> 'p predicate_binding
(** Make a predicate binding for a nullary predicate of the given name
    converting to the given predicate. *)

val unary_lift :
  name: string ->
  converter: 'i t ->
  (('i Formula.t -> 'p) * ('p -> 'i Formula.t option)) ->
  'p predicate_binding
(** Make a unary predicate that lifts other formulas. The argument is converted
    using the [converter] and the result is passed to the given lifting
    function. *)

val unary_string :
  name: string ->
  ((string -> 'p) * ('p -> string option)) ->
  'p predicate_binding
(** Make a unary predicate whose argument must be a string. *)

val unary_int :
  name: string ->
  ((int -> 'p) * ('p -> int option)) ->
  'p predicate_binding
(** Make a unary predicate whose argument must be an int. *)

val unary_raw :
  name: string ->
  cast: ((string -> 'i option) * ('i -> string)) ->
  type_: string ->
  (('i -> 'p) * ('p -> 'i option)) ->
  'p predicate_binding
(** Make a unary predicate whose argument must be raw. The raw string is then
    passed to [~cast] to convert it to an ['i]ntermediary value. If it fails, an
    error message is created using [~type_]. For instance, {!unary_int} is
    [unary_raw ~cast:int_of_string_opt ~type_:"int"]. *)

(** {2 Building} *)

val make :
  ?raw: (string -> ('p Formula.t, string) Result.t) ->
  'p predicate_binding list ->
  'p t
(** Make a converter from a list of {!predicate_binding}s and a [~raw] argument.
    This argument will be called on standalone strings. For instance, in the
    formula ["bonjour baguette:oui monsieur :chocolat"], the [~raw] function
    will be called on ["bonjour"] and ["monsieur"] while ["baguette"] and
    ["chocolat"] will be searched for in the {!predicate_binding} list. *)

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
