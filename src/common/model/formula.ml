(** {1 Formula} *)

open Nes

type 'filter t =
  | False
  | True
  | Not of 'filter t
  | And of 'filter t * 'filter t
  | Or  of 'filter t * 'filter t
  | Pred of 'filter
[@@deriving yojson]

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

let interpet_and_l = function
  | [] -> interpret_true
  | h :: t -> List.fold_left interpret_and h t

let interpet_or_l = function
  | [] -> interpret_false
  | h :: t -> List.fold_left interpret_or h t

let interpret_exists p l =
  Lwt_list.map_s p l >|=| interpet_or_l

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
