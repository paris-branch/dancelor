(** {1 TextFormula} Textual Representation of formulas. *)

(* To avoid cycles, the [TextFormula] module is split in two. [TextFormulaType]
   contains only the types, because they are necessary for the lexing, parsing
   and printing. This module contains the actual functions. *)

open Nes
open Formula
include TextFormulaType

module Lexer = TextFormulaLexer
module Printer = TextFormulaPrinter

module Parser = struct
  module L = MenhirLib.LexerUtil
  module E = MenhirLib.ErrorReports
  module I = TextFormulaParser.MenhirInterpreter

  exception ParseError of Lexing.position * Lexing.position * string

  let parse_error_fmt start end_ =
    Format.kasprintf
    @@ fun msg ->
      raise (ParseError (start, end_, msg))

  let parse_error start end_ msg =
    parse_error_fmt start end_ "%s" msg

  let show text positions =
    E.extract text positions
    |> E.sanitize
    |> E.compress
    |> E.shorten 20 (* max width 43 *)

  let env checkpoint =
    match checkpoint with
    | I.HandlingError env -> env
    | _ -> assert false

  let state checkpoint : int =
    match I.top (env checkpoint) with
    | Some (I.Element (s, _, _, _)) -> I.number s
    | None -> 0 (* FIXME *)

  let succeed v = Ok v

  let fail text buffer _checkpoint =
    let (start, end_) = E.last buffer in
    let indication = E.show (show text) buffer in
    Error (start, end_, indication)

  let from_string ?(filename = "-") text =
    let lexbuf = L.init filename (Lexing.from_string text) in
    let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let (buffer, supplier) = E.wrap_supplier supplier in
    let checkpoint = TextFormulaParser.Incremental.formula lexbuf.lex_curr_p in
    I.loop_handle succeed (fail text buffer) supplier checkpoint

  let from_string_exn ?filename text =
    match from_string ?filename text with
    | Ok v -> v
    | Error (start, end_, msg) -> parse_error start end_ msg
end
let from_string = Parser.from_string_exn

let pp = Printer.pp

(******************************************************************************)
(* To help convert from text_formula to filter. *)

type 'a or_error = ('a , string ) result
let error_fmt fmt = Format.kasprintf (fun s -> Error s) fmt

type 'a raw_builder = string -> 'a Formula.t or_error
type 'a nullary_builder = 'a Formula.t
type 'a nullary_text_predicates = (string * 'a nullary_builder ) list
type 'a unary_builder = t -> 'a Formula.t or_error
type 'a unary_text_predicates = (string * 'a unary_builder ) list

let to_formula predicate_to_formula formula =
  let rec to_formula = function
    | False -> Ok False
    | True -> Ok True
    | Not formula ->
      (
        match to_formula formula with
        | Ok formula -> Ok (Not formula)
        | Error err -> Error err
      ) (* FIXME: ppx_syntext *)
    | And (formula1, formula2) ->
      (
        match to_formula formula1,
        to_formula formula2 with
        | Ok formula1, Ok formula2 -> Ok (And (formula1, formula2))
        | Error err, _ | _, Error err -> Error err
      )
    | Or (formula1, formula2) ->
      (
        match to_formula formula1,
        to_formula formula2 with
        | Ok formula1, Ok formula2 -> Ok (Or (formula1, formula2))
        | Error err, _ | _, Error err -> Error err
      )
    | Pred pred ->
      predicate_to_formula pred
  in
  to_formula formula

let make_predicate_to_formula
    (raw_builder : 'a raw_builder)
    (nullary_text_predicates : 'a nullary_text_predicates)
    (unary_text_predicates : 'a unary_text_predicates)
  = function
  | Raw string ->
    raw_builder string
  | Nullary pred ->
    (
      match List.assoc_opt pred nullary_text_predicates with
      | None -> error_fmt "the nullary predicate \":%s\" does not exist" pred
      | Some pred -> Ok pred
    )
  | Unary (pred, sub_formula) ->
    (
      match List.assoc_opt pred unary_text_predicates with
      | None -> error_fmt "the unary predicate \"%s:\" does not exist" pred
      | Some mk_pred -> mk_pred sub_formula
    )

let make_to_formula
    (raw_builder : 'a raw_builder)
    (nullary_text_predicates : 'a nullary_text_predicates)
    (unary_text_predicates : 'a unary_text_predicates)
    (formula : t)
    : 'a Formula.t or_error
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
    ~(convert : string -> ('a , string ) result)
    (mk : 'a -> 'b Formula.t)
    : t -> ( 'b Formula.t , string ) result
  = function
  | Pred (Raw s) ->
    (
      match convert s with
      | Ok s -> Ok (mk s)
      | Error err -> Error err
    )
  | _ -> Error "this predicate only accepts raw arguments"

let no_convert x = Ok x

let convert_int s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None -> Error "this predicate only accepts integers"

let rec predicates from_predicate = function
  | False -> String.Set.empty
  | True -> String.Set.empty
  | Not formula -> predicates from_predicate formula
  | And (formula1, formula2) | Or (formula1, formula2) ->
    String.Set.union
      (predicates from_predicate formula1)
      (predicates from_predicate formula2)
  | Pred pred ->
    match from_predicate pred with
    | None -> String.Set.empty
    | Some string -> String.Set.singleton string

let nullary_predicates =
  predicates
  @@ function
  | Nullary string -> Some string
  | _ -> None

let unary_predicates =
  predicates
  @@ function
  | Unary (string, _) -> Some string
  | _ -> None
