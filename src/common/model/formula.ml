(** {1 Formula} *)

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





(* FIXME: put that somewhere at some point *)

type text_formula = text_predicate t

and text_predicate =
  | Raw of string
  | App of string * text_formula

let raw s = Pred (Raw s)
let app p e = Pred (App (p, e))
