open Nes
open Formula
open TextFormulaType

let level = function
  | False | True | Pred _ -> 3
  | Not _ -> 2
  | And _ -> 1
  | Or _ -> 0

let pp_raw fmt string =
  match String.index_opt string ' ' with
  | None -> Format.pp_print_string fmt string
  | Some _ -> fpf fmt "\"%s\"" string
                (* FIXME: escape quotes *)

let rec pp fmt formula =
  match formula with
  | False -> fpf fmt ":false"
  | True -> fpf fmt ":true"
  | Not f -> fpf fmt ":not %a" (pp_pars formula) f
  | And (f1, f2) -> fpf fmt "%a :and %a" (pp_pars formula) f1 (pp_pars formula) f2
  | Or  (f1, f2) -> fpf fmt "%a :or %a" (pp_pars formula) f1 (pp_pars formula) f2
  | Pred pred -> pp_predicate formula fmt pred

and pp_predicate parent fmt = function
  | Raw string -> pp_raw fmt string
  | Nullary pred -> fpf fmt ":%s" pred
  | Unary (pred, f) -> fpf fmt "%s:%a" pred (pp_pars parent) f

and pp_pars parent fmt formula =
  if level parent > level formula then
    fpf fmt "(%a)" pp formula
  else
    pp fmt formula
