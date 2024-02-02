(** {1 TextFormula} Textual Representation of formulas. *)

(* To avoid cycles, the [TextFormula] module is split in two. [TextFormulaType]
   contains only the types, because they are necessary for the lexing, parsing
   and printing. This module contains the actual functions. *)

open Nes
module Converter = TextFormulaConverter
module Lexer = TextFormulaLexer
module Printer = TextFormulaPrinter

include TextFormulaType

module Parser = struct
  module L = MenhirLib.LexerUtil
  module E = MenhirLib.ErrorReports
  module I = TextFormulaParser.MenhirInterpreter

  (* exception ParseError of Lexing.position * Lexing.position * string *)

  (* let parse_error_fmt start end_ = *)
  (*   Format.kasprintf @@ fun msg -> *)
  (*   raise (ParseError (start, end_, msg)) *)

  (* let parse_error start end_ msg = *)
  (*   parse_error_fmt start end_ "%s" msg *)

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

  let from_string_internal ?(filename="-") text =
    let lexbuf = L.init filename (Lexing.from_string text) in
    let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let (buffer, supplier) = E.wrap_supplier supplier in
    let checkpoint = TextFormulaParser.Incremental.formula lexbuf.lex_curr_p in
    I.loop_handle succeed (fail text buffer) supplier checkpoint

  let from_string ?filename text =
    match from_string_internal ?filename text with
    | Ok formula -> Ok formula
    | Error (_, _, where) -> kspf Result.error "There is a syntax error %s in your request." where
    | exception Lexer.UnexpectedCharacter char ->
      kspf Result.error ("There is an unexpected character in your request: '%c'. "
                         ^^ "If you really want to type it, protect it with quotes, "
                         ^^ "eg. \"foo%cbar\".") char char
    | exception Lexer.UnterminatedQuote ->
      kspf Result.error ("There is an unterminated quote in your request. "
                         ^^ "If you just want to type a quote character, "
                         ^^ "whether inside quotes or not, escape it, eg. \"foo\\\"bar\".")
end
let from_string = Parser.from_string

let pp = Printer.pp
let to_string = Printer.to_string

let to_formula = Converter.to_formula
