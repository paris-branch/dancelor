open Nes
open Formula
module Type = TextFormulaType

type 'p predicate =
  | Nullary of 'p Formula.t
  | Unary of (Type.t -> ('p Formula.t, string) Result.t)

let map_predicate f = function
  | Nullary formula -> Nullary (f formula)
  | Unary to_formula -> Unary (Result.map f % to_formula)

type 'p predicate_binding = string * 'p predicate

let nullary' ~name formula = (name, Nullary formula)
let nullary ~name predicate = nullary' ~name (Formula.pred predicate)

let unary' ~name to_formula = (name, Unary to_formula)
let unary ~name to_predicate = unary' ~name (Result.map Formula.pred % to_predicate)

let map_predicate_binding f (name, predicate) =
  (name, map_predicate f predicate)

type 'p t = {
  raw: string -> ('p Formula.t, string) Result.t;
  predicates: 'p predicate String.Map.t
}

let raw converter = converter.raw
let predicates converter = List.of_seq @@ String.Map.to_seq converter.predicates

let make ~raw predicates = {raw; predicates = String.Map.of_seq (List.to_seq predicates)}

let map_raw f raw = Result.map f % raw

let map f {raw; predicates} =
  {
    raw = map_raw f raw;
    predicates = String.Map.map (map_predicate f) predicates
  }

let predicate_to_formula converter text_predicate =
  match text_predicate with
  | Type.Raw string -> converter.raw string
  | Type.Nullary name | Type.Unary (name, _) ->
    Option.fold
      (String.Map.find_opt name converter.predicates)
      ~none: (kspf Result.error "the predicate \"%s\" does not exist" name)
      ~some: (fun predicate ->
          match text_predicate, predicate with
          | Type.Nullary _, Nullary formula -> Ok formula
          | Type.Unary (_, subformula), Unary to_formula -> to_formula subformula
          | Type.Nullary _, _ -> kspf Result.error "the predicate \":%s\" expects no argument" name
          | Type.Unary _, _ -> kspf Result.error "the predicate \"%s:\" expects one argument" name
          | Type.Raw _, _ -> assert false
        )

let to_formula converter = Formula.convert (predicate_to_formula converter)

let unary_raw ~name ~cast ~type_ to_predicate = unary ~name @@ function
  | Pred (Raw s) ->
    Result.map to_predicate
      (Option.to_result
         ~none: (spf "the unary predicate \"%s:\" only accepts %s arguments" name type_)
         (cast s))
  | _ ->
    Error (spf "the unary predicate \"%s:\" only accepts a %s arguments" name type_)

let unary_string = unary_raw ~cast:Option.some ~type_:"string"
let unary_int = unary_raw ~cast:int_of_string_opt ~type_:"int"

let rec predicate_names predicate_name = function
  | False -> String.Set.empty
  | True -> String.Set.empty
  | Not formula -> predicate_names predicate_name formula
  | And (formula1, formula2) | Or (formula1, formula2) ->
    String.Set.union
      (predicate_names predicate_name formula1)
      (predicate_names predicate_name formula2)
  | Pred pred ->
    Option.fold
      ~none: String.Set.empty
      ~some: String.Set.singleton
      (predicate_name pred)

let merge converter1 converter2 =
  let formula_result_or_ result1 result2 =
    (* If both formulas are [Ok], then [Formula.or_]. If only one is [Ok], then
       that one. If both are [Error] then [Error]. *)
    match result1, result2 with
    | Ok formula1, Ok formula2 -> Ok (Formula.or_ formula1 formula2)
    | Ok formula1, _ -> Ok formula1
    | _, Ok formula2 -> Ok formula2
    | Error err1, Error err2 -> Error (err1 ^ "\n" ^ err2)
  in
  let raw string =
    formula_result_or_
      (converter1.raw string)
      (converter2.raw string)
  in
  let predicates =
    String.Map.union
      (fun name predicate1 predicate2 ->
         match predicate1, predicate2 with
         | Nullary formula1, Nullary formula2 ->
           Some (Nullary (Formula.or_ formula1 formula2))
         | Unary to_formula1, Unary to_formula2 ->
           Some (Unary (fun text_formula -> formula_result_or_ (to_formula1 text_formula) (to_formula2 text_formula)))
         | _ ->
           kspf invalid_arg "TextFormula.Converter.merge: predicate \"%s\" appears in both with different arities" name)
      converter1.predicates
      converter2.predicates
  in
  { raw; predicates }

let merge_l = function
  | [] -> invalid_arg "TextFormula.Converter.merge_l"
  | converter :: converters -> List.fold_left merge converter converters
