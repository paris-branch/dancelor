open Nes
open Formula
open TextFormulaType

type 'a or_error = ('a, string) result
let error_fmt fmt = Format.kasprintf (fun s -> Error s) fmt

type 'a raw_builder = string -> 'a Formula.t or_error
type 'a nullary_builder = 'a Formula.t
type 'a nullary_text_predicates = (string * 'a nullary_builder) list
type 'a unary_builder = t -> 'a Formula.t or_error
type 'a unary_text_predicates = (string * 'a unary_builder) list

let make_predicate_to_formula
    (raw_builder : 'a raw_builder)
    (nullary_text_predicates : 'a nullary_text_predicates)
    (unary_text_predicates : 'a unary_text_predicates)
  =
  function
  | Raw string ->
    raw_builder string
  | Nullary pred ->
    (match List.assoc_opt pred nullary_text_predicates with
     | None -> error_fmt "the nullary predicate \":%s\" does not exist" pred
     | Some pred -> Ok pred)
  | Unary (pred, sub_formula) ->
    (match List.assoc_opt pred unary_text_predicates with
     | None -> error_fmt "the unary predicate \"%s:\" does not exist" pred
     | Some mk_pred -> mk_pred sub_formula)

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
  Formula.convert predicate_to_formula formula

(** Helper to build a unary predicate whose argument must be raw only. *)
let raw_only
    ~(convert : string -> ('a, string) result)
    (mk : 'a -> 'b Formula.t)
  : t -> ('b Formula.t, string) result
  =
  function
  | Pred (Raw s) ->
    (match convert s with
     | Ok s -> Ok (mk s)
     | Error err -> Error err)
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
  predicates @@ function
  | Nullary string -> Some string
  | _ -> None

let unary_predicates =
  predicates @@ function
  | Unary (string, _) -> Some string
  | _ -> None
