open Nes
open Formula
open TextFormulaType

let level = function
  | False | True | Pred _ -> 3
  | Not _ -> 2
  | And _ -> 1
  | Or _ -> 0

let escape_char = function
  | '"' -> Seq.(cons '\\' (return '"'))
  | '\\' -> Seq.(cons '\\' (return '\\'))
  | c -> Seq.return c

let escape = function
  | "" -> "\"\""
  | s when String.exists is_special s -> "\"" ^ String.of_seq (Seq.concat_map escape_char (String.to_seq s)) ^ "\""
  | s -> s

let pp_raw fmt string =
  if String.exists is_special string then
    fpf fmt "\"%s\"" (escape string)
  else
    fpf fmt "%s" string

let rec pp fmt formula =
  match formula with
  | False -> fpf fmt ":false"
  | True -> fpf fmt ":true"
  | Not f -> fpf fmt ":not %a" (pp_pars formula) f
  | And (f1, f2) -> fpf fmt "%a :and %a" (pp_pars formula) f1 (pp_pars formula) f2
  | Or  (f1, f2) -> fpf fmt "%a :or %a" (pp_pars formula) f1 (pp_pars formula) f2
  | Pred pred -> pp_predicate formula fmt pred

and pp_predicate parent fmt = function
  | Raw string -> fpf fmt "%s" (escape string)
  | Nullary pred -> fpf fmt ":%s" pred
  | Unary (pred, f) -> fpf fmt "%s:%a" pred (pp_pars parent) f

and pp_pars parent fmt formula =
  if level parent > level formula then
    fpf fmt "(%a)" pp formula
  else
    pp fmt formula

let pp_predicate = pp_predicate False

let to_string = Format.kasprintf Fun.id "%a" pp
