open Nes

type 'filter t =
  | False
  | True
  | Not of 'filter t
  | And of 'filter t * 'filter t
  | Or  of 'filter t * 'filter t
  | Pred of 'filter
[@@deriving yojson]

(** For debugging purposes, our custom [show]. *)
let pp pp_pred fmt formula =
  let ppf par = kaspf (fun s -> fpf fmt "%s%s%s" (if par then "(" else "") s (if par then ")" else "")) in
  let rec pp above fmt = function
    | False -> ppf false "⊥"
    | True -> ppf false "⊤"
    | Not f -> ppf false "¬ %a" (pp `Not) f
    | And (f1, f2) -> ppf (above = `Or || above = `Not) "%a ∧ %a" (pp `And) f1 (pp `And) f2
    | Or (f1, f2) -> ppf (above = `And || above = `Not) "%a ∨ %a" (pp `Or) f1 (pp `Or) f2
    | Pred p -> pp_pred fmt p
  in
  fpf fmt "(%a)" (pp `Root) formula

let false_ = False
let true_ = True

let not_ f = Not f

let and_ f1 f2 = And(f1, f2)
let and_l = function
  | [] -> True
  | h::t -> List.fold_left and_ h t

let or_ f1 f2 = Or(f1, f2)
let or_l = function
  | [] -> False
  | h::t -> List.fold_left or_ h t

let pred value = Pred value

let interpret_false = 0.
let interpret_true = 1.

let interpret_not s = 1. -. s
let interpret_and s1 s2 = min s1 s2
let interpret_or s1 s2 = max s1 s2

let interpret_and_l = function
  | [] -> interpret_true
  | h :: t -> List.fold_left interpret_and h t

let interpret_or_l = function
  | [] -> interpret_false
  | h :: t -> List.fold_left interpret_or h t

let interpret_exists p l =
  Lwt_list.map_s p l >|=| interpret_or_l

let interpret_bool b =
  if b then interpret_true else interpret_false

let interpret formula interpret_predicate =
  let rec interpret = function
    | False -> Lwt.return interpret_false
    | True -> Lwt.return interpret_true
    | Not formula ->
      let%lwt score = interpret formula in
      Lwt.return (interpret_not score)
    | And (formula1, formula2) ->
      let%lwt score1 = interpret formula1
      and     score2 = interpret formula2 in
      Lwt.return (interpret_and score1 score2)
    | Or (formula1, formula2) ->
      let%lwt score1 = interpret formula1
      and     score2 = interpret formula2 in
      Lwt.return (interpret_or score1 score2)
    | Pred predicate ->
      interpret_predicate predicate
  in
  interpret formula

let rec convert f = function
  | False -> Ok False
  | True -> Ok True
  | Not e -> Result.map (fun e' -> Not e') (convert f e)
  | And (e1, e2) -> Result.bind (convert f e1) (fun e1' -> Result.map (fun e2' -> And (e1', e2')) (convert f e2))
  | Or (e1, e2) -> Result.bind (convert f e1) (fun e1' -> Result.map (fun e2' -> Or (e1', e2')) (convert f e2))
  | Pred pred -> f pred

module Make_Serialisable (M : Madge_common.SERIALISABLE) = struct
  type nonrec t = M.t t

  let _key = M._key ^ "-filter"

  let of_yojson = of_yojson M.of_yojson
  let to_yojson = to_yojson M.to_yojson
end
