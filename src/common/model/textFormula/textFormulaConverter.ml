open Nes
open Formula
open TextFormulaType

(* REVIEW: Should maybe go to [NesResult]. *)
let error_fmt fmt = Format.kasprintf (fun s -> Error s) fmt

type 'p raw_builder = string -> ('p Formula.t, string) Result.t
(** A raw builder describes how to get a formula from raw strings. Raw strings
    are search strings not attached to a predicate. For instance, in the search
    string ["bonjour baguette:oui monsieur :chocolate"], the raw strings are
    ["bonjour"] and ["monsieur"]. *)

type 'p nullary_predicate = {
  name: string;
  formula: 'p Formula.t;
}
(** A nullary text predicate is described by its name and a formula to which it
    should be converted. For instance, in the search string ["bonjour
    baguette:oui monsieur :chocolate"], the nullary predicate is [:chocolate]
    which could, in an imaginary world, be described as [{ name = "chocolate";
    formula = And(Food, Kind "choco") }]. *)

let nullary ~name formula = {name; formula}
(** Smart constructor of a {!nullary_predicate}. *)

let name_nullary {name; formula=_} = name

let map_nullary f {name; formula} = {name; formula = f formula}
(** Map over the formula in a nullary predicate. *)

type 'p nullary_predicates = 'p nullary_predicate list
(** Several nullary predicates; the names should be all distinct. Although not
    necessary, ideally, they should also be distinct fomr the ones in
    {!unary_predicates}. *)

type 'p unary_predicate = {
  name: string;
  to_formula: t -> ('p Formula.t, string) Result.t;
}
(** A unary text predicate is described by its name and a function from its
    argument (as a text formula) to a formula representing this applied unary
    predicate. For instance, in the search string ["bonjour baguette:oui
    monsieur :chocolate"], the unary predicate is [baguette:], applied to [Raw
    "oui"] which could, in an imaginary world, be described as [{ name =
    "baguette"; to_formula = (function "oui" -> Ok true | "non" -> Ok false | _
    -> Error "bad french!") }] *)

let unary ~name to_formula = {name; to_formula}
(** Smart constructor of a {!unary_predicate}. *)

let name_unary {name; to_formula=_} = name

let map_unary f {name; to_formula} = {name; to_formula = f to_formula}
(** Map over the [to_formula] field of a unary predicate. *)

type 'p unary_predicates = 'p unary_predicate list
(** Several unary predicates; the names should be all distinct. Although not
    necessary, ideally, they should also be distinct from the ones in
    {!nullary_predicates}. *)

let make_predicate_to_formula
    (raw_builder : 'p raw_builder)
    (nullary_predicates : 'p nullary_predicates)
    (unary_predicates : 'p unary_predicates)
  : predicate -> ('p Formula.t, string) Result.t
  =
  function
  | Raw string -> raw_builder string
  | Nullary n ->
    Option.fold
      (List.find_opt (fun (np : 'p nullary_predicate) -> np.name = n) nullary_predicates)
      ~some: (fun np -> Ok np.formula)
      ~none: (error_fmt "the nullary predicate \":%s\" does not exist" n)
  | Unary (n, e) ->
    Option.fold
      (List.find_opt (fun (up : 'p unary_predicate) -> up.name = n) unary_predicates)
      ~some: (fun up -> up.to_formula e)
      ~none: (error_fmt "the unary predicate \"%s:\" does not exist" n)

let make_to_formula
    (raw_builder : 'p raw_builder)
    (nullary_predicates : 'p nullary_predicates)
    (unary_predicates : 'p unary_predicates)
  : t -> ('p Formula.t, string) Result.t
  =
  Formula.convert @@ make_predicate_to_formula raw_builder nullary_predicates unary_predicates

(** Helper to build a unary predicate whose argument must be raw only. *)
let raw_only
    ~(convert : string -> ('a, string) result)
    (mk : 'a -> 'b Formula.t)
  : t -> ('b Formula.t, string) result
  =
  function
  | Pred (Raw s) -> Result.map mk (convert s)
  | _ -> Error "this predicate only accepts raw arguments"

let no_convert x = Ok x

let convert_int s =
  Option.to_result (int_of_string_opt s)
    ~none: "this predicate only accepts integers"

let rec predicates from_predicate = function
  | False -> String.Set.empty
  | True -> String.Set.empty
  | Not formula -> predicates from_predicate formula
  | And (formula1, formula2) | Or (formula1, formula2) ->
    String.Set.union
      (predicates from_predicate formula1)
      (predicates from_predicate formula2)
  | Pred pred ->
    Option.fold
      ~none: String.Set.empty
      ~some: String.Set.singleton
      (from_predicate pred)

let nullary_predicates = predicates @@ function Nullary string -> Some string | _ -> None
let unary_predicates = predicates @@ function Unary (string, _) -> Some string | _ -> None
