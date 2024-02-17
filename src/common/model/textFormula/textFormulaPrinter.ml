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

let head = function
  | False -> `False
  | True -> `True
  | Not _ -> `Not
  | And _ -> `And
  | Or _ -> `Or
  | Pred _ -> `Pred

let needs_parentheses ~parent head = match (parent, head) with
  | (`False, _) | (`True, _) -> assert false
  | (`Not, `And) | (`Not, `Or) -> true
  | (`And, `Or) | (`Or, `And) -> true
  | (`Pred, `Not) | (`Pred, `And) | (`Pred, `Or) -> true
  | _ -> false

let rec pp fmt = function
  | False -> fpf fmt ":false"
  | True -> fpf fmt ":true"
  | Not f -> fpf fmt ":not %a" (pp_pars `Not) f
  | And (f1, f2) -> fpf fmt "%a %a" (pp_pars `And) f1 (pp_pars `And) f2
  | Or  (f1, f2) -> fpf fmt "%a :or %a" (pp_pars `Or) f1 (pp_pars `Or) f2
  | Pred pred -> pp_predicate fmt pred

and pp_predicate fmt = function
  | Raw string -> fpf fmt "%s" (escape string)
  | Nullary pred -> fpf fmt ":%s" pred
  | Unary (pred, f) -> fpf fmt "%s:%a" pred (pp_pars `Pred) f

and pp_pars parent fmt formula =
  if needs_parentheses ~parent (head formula) then
    fpf fmt "(%a)" pp formula
  else
    pp fmt formula

let pp_predicate = pp_predicate

let to_string = Format.kasprintf Fun.id "%a" pp
