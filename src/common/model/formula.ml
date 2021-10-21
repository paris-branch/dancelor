(** {1 Formula} *)

include FormulaType

let interpret_score formula interpret_predicate =
  let rec interpret = function
    | False -> Lwt.return 0.
    | True -> Lwt.return 1.
    | Not formula ->
      let%lwt score = interpret formula in
      Lwt.return (1. -. score)
    | And (formula1, formula2) ->
      let%lwt score1 = interpret formula1
      and     score2 = interpret formula2 in
      Lwt.return (score1 *. score2)
    | Or (formula1, formula2) ->
      interpret (Not (And (Not formula1, Not formula2)))
    | Pred predicate ->
      interpret_predicate predicate
  in
  interpret formula

let interpret formula interpret_predicate =
  let%lwt result =
    interpret_score formula @@ fun predicate ->
    if%lwt interpret_predicate predicate then
      Lwt.return 1.
    else
      Lwt.return 0.
  in
  assert (result = 1. || result = 0.);
  Lwt.return (result = 1.)

let fpf = Format.fprintf

let rec pp pp_pred fmt = function
  | False -> fpf fmt "false"
  | True -> fpf fmt "true"
  | Not f -> fpf fmt "not (%a)" (pp pp_pred) f
  | And (f1, f2) -> fpf fmt "(%a) && (%a)" (pp pp_pred) f1 (pp pp_pred) f2
  | Or  (f1, f2) -> fpf fmt "(%a) || (%a)" (pp pp_pred) f1 (pp pp_pred) f2
  | Pred pred -> pp_pred fmt pred

let pp_opaque fmt f =
  pp (fun fmt _ -> fpf fmt "<opaque>") fmt f

(** {2 Serialisable} *)

module Make_Serialisable (M : Madge_common.SERIALISABLE) = struct
  type nonrec t = M.t t

  let _key = M._key ^ "-filter"

  let of_yojson = of_yojson M.of_yojson
  let to_yojson = to_yojson M.to_yojson
end

let pp_text_formula = FormulaPrinter.pp

let text_formula_from_string string =
  FormulaParser.formula FormulaLexer.token (Lexing.from_string string)
(* FIXME: use incremental parsing to get a proper error reporting *)
(* return a result type because there will be errors often and *)
(* we want the client code to handle them *)
