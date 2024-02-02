open Nes

(** {1 Text formula converter}

    This module contains utilities to help convert text formulas into non-text
    formulas. *)

type 'p t
(** Abstract type of a converter from text formulas to formulas with predicates
    of type ['p]. *)

(** {2 Building} *)

type 'p predicate_binding
(** Abstract type of a binding in the map of predicates. *)

val nullary : name:string -> 'p -> 'p predicate_binding
(** Make a predicate binding for a nullary predicate of the given name
    converting to the given predicate. *)

val unary : name:string -> (TextFormulaType.t -> ('p, string) Result.t) -> 'p predicate_binding
(** Make a predicate binding for a unary predicate of the given name converter
    to the given predicate. *)

val make :
  raw: (string -> ('p Formula.t, string) Result.t) ->
  'p predicate_binding list ->
  'p t
(** Make a converter from a list of {!predicate_binding}s and a [~raw] argument.
    This argument will be called on standalone strings. For instance, in the
    formula ["bonjour baguette:oui monsieur :chocolat"], the [~raw] function
    will be called on ["bonjour"] and ["monsieur"] while ["baguette"] and
    ["chocolat"] will be searched for in the {!predicate_binding} list. *)

(** {2 Using} *)

val predicate_to_formula : 'p t -> TextFormulaType.predicate -> ('p Formula.t, string) Result.t
(** Convert one text predicate to a formula using the given converter. Can fail
    via [Result] if, for instance, the predicates are not used with the right
    arity. *)

val to_formula : 'p t -> TextFormulaType.t -> ('p Formula.t, string) Result.t
(** Convert a text formula to a formula using the given converter. This is
    basically {!Formula.convert} applied to {!predicate_to_formula}. *)

(** {2 Mapping} *)

val map_predicate_binding :
  ('p Formula.t -> 'q Formula.t) ->
  'p predicate_binding ->
  'q predicate_binding
(** Map the given function on a predicate binding. The result is another
    predicate binding with formulas transformed using the given function. *)

val map : ('p Formula.t -> 'q Formula.t) -> 'p t -> 'q t
(** Map over a converter given a function. *)

(** {2 Other helpers} *)

val unary_raw : name: string -> (string -> ('p, string) Result.t) -> 'p predicate_binding
(** Build a unary predicate whose argument must be raw only. *)

val unary_int : name: string -> (int -> ('p, string) Result.t) -> 'p predicate_binding
(** Build a unary predicate whose argument must be an int. *)

val raw : 'p t -> string -> ('p Formula.t, string) Result.t
(** Return the raw function of a converter. *)

val predicates : 'p t -> 'p predicate_binding list
(** Return the bindings of a converter. *)

val predicate_names : ('p -> string option) -> 'p Formula.t -> String.Set.t
(** All the predicate names that appear in a formula. *)
(* FIXME: Maybe it is only meant to be used for text formulas? *)

val merge : 'p t -> 'p t -> 'p t
(** Merge two converters together. Predicates that exist on both sides must have
    the same arity. The result is then the disjunction of the two formulas.
    Raises {!Invalid_arg} if a predicate exists in both with different
    arities. *)

val merge_l : 'p t list -> 'p t
(** Same as {!merge} on a non-empy list. *)
