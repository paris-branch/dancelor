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

type text_formula = text_predicate t

and text_predicate =
  | Raw of string
  | Nullary of string
  | Unary of string * text_formula

let raw s = Pred (Raw s)
let nullary p = Pred (Nullary p)
let unary p e = Pred (Unary (p, e))
