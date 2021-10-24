(** {1 TextFormula} Textual Representation of formulas. *)

(* To avoid cycles, the [TextFormula] module is split in two. [TextFormulaType]
   contains only the types, because they are necessary for the lexing, parsing
   and printing. This module contains the actual functions. *)

open Formula
include TextFormulaType

let from_string string =
  TextFormulaParser.formula TextFormulaLexer.token (Lexing.from_string string)
(* FIXME: use incremental parsing to get a proper error reporting *)
(* return a result type because there will be errors often and *)
(* we want the client code to handle them *)

let pp = TextFormulaPrinter.pp

(******************************************************************************)
(* To help convert from text_formula to filter. *)

type 'a raw_builder = string -> 'a Formula.t
type 'a nullary_builder = 'a Formula.t
type 'a nullary_text_predicates = (string * 'a nullary_builder) list
type 'a unary_builder = t -> 'a Formula.t
type 'a unary_text_predicates = (string * 'a unary_builder) list

let to_formula predicate_to_formula formula =
  let rec to_formula = function
    | False -> False
    | True -> True
    | Not formula ->
      Not (to_formula formula)
    | And (formula1, formula2) ->
      And (to_formula formula1,
           to_formula formula2)
    | Or (formula1, formula2) ->
      Or (to_formula formula1,
          to_formula formula2)
    | Pred pred ->
      predicate_to_formula pred
  in
  to_formula formula

let make_predicate_to_formula
    (raw_builder : 'a raw_builder)
    (nullary_text_predicates : 'a nullary_text_predicates)
    (unary_text_predicates : 'a unary_text_predicates)
  =
  function
  | Raw string ->
    raw_builder string
  | Nullary pred ->
    (match List.assoc_opt pred nullary_text_predicates with
     | None -> false_ (* FIXME: do we want to raise an error? *)
     | Some pred -> pred)
  | Unary (pred, sub_formula) ->
    (match List.assoc_opt pred unary_text_predicates with
     | None -> false_ (* FIXME: do we want to raise an error? *)
     | Some mk_pred -> mk_pred sub_formula)

let make_to_formula
    (raw_builder : 'a raw_builder)
    (nullary_text_predicates : 'a nullary_text_predicates)
    (unary_text_predicates : 'a unary_text_predicates)
    (formula : t)
  : 'a Formula.t
  =
  let predicate_to_formula =
    make_predicate_to_formula
      raw_builder
      nullary_text_predicates
      unary_text_predicates
  in
  to_formula predicate_to_formula formula

(** Helper to build a unary predicate whose argument must be raw only. *)
let raw_only
    ~(convert : string -> 'a)
    (mk : 'a -> 'b Formula.t)
  : t -> 'b Formula.t
  =
  function
  | Pred (Raw s) -> mk (convert s)
  | _ -> false_ (* FIXME: do we want to raise an error? *)
