open Nes
open Formula
module Type = TextFormulaType

(* NOTE: Even though this type is called [to_predicate], and even though all our
   builders ([nullary], [unary_lift], etc.) work on predicates, we need to carry
   formulas because of things like [merge]. *)
type 'p to_predicate =
  | Nullary of 'p Formula.t
  | Unary of (Type.t -> ('p Formula.t, string) Result.t)

let map_to_predicate f = function
  | Nullary formula -> Nullary (f formula)
  | Unary to_formula -> Unary (Result.map f % to_formula)

type 'p t = {
  raw: string -> ('p Formula.t, string) Result.t;
  to_predicates: 'p to_predicate String.Map.t
}

type 'p predicate_binding = string * 'p to_predicate

let make_predicate_binding ~name ~to_predicate =
  (name, to_predicate)

let nullary ~name predicate =
  make_predicate_binding ~name ~to_predicate:(Nullary (Formula.pred predicate))

let unary ~name to_predicate =
  make_predicate_binding ~name ~to_predicate:(Unary (Result.map Formula.pred % to_predicate))

let make ?raw predicates_bindings =
  {
    raw = Option.value ~default:(fun _ -> Error "raw arguments are unsupported") raw;
    to_predicates = String.Map.of_seq (List.to_seq predicates_bindings);
  }

let map f {raw; to_predicates} =
  {
    raw = Result.map f % raw;
    to_predicates = String.Map.map (map_to_predicate f) to_predicates
  }

let predicate_to_formula converter text_predicate =
  match text_predicate with
  | Type.Raw string -> converter.raw string
  | Type.Nullary name | Type.Unary (name, _) ->
    Option.fold
      (String.Map.find_opt name converter.to_predicates)
      ~none: (kspf Result.error "the predicate \"%s\" does not exist" name)
      ~some: (fun to_predicate ->
          match text_predicate, to_predicate with
          | Type.Nullary _, Nullary formula -> Ok formula
          | Type.Unary (_, subformula), Unary to_formula -> to_formula subformula
          | Type.Nullary _, _ -> kspf Result.error "the predicate \":%s\" expects no argument" name
          | Type.Unary _, _ -> kspf Result.error "the predicate \"%s:\" expects one argument" name
          | Type.Raw _, _ -> assert false
        )

let to_formula converter = Formula.convert (predicate_to_formula converter)

let unary_lift ~name ~converter (lift, _unlift) =
  unary ~name (Result.map lift % to_formula converter)

let unary_raw ~name ~cast:(cast, _uncast) ~type_ (to_predicate, _from_predicate) =
  unary ~name @@ function
  | Pred (Raw s) ->
    Result.map to_predicate
      (Option.to_result
         ~none: (spf "the unary predicate \"%s:\" only accepts %s arguments" name type_)
         (cast s))
  | _ ->
    Error (spf "the unary predicate \"%s:\" only accepts a %s arguments" name type_)

let unary_string = unary_raw ~cast:(Option.some, Fun.id) ~type_:"string"
let unary_int = unary_raw ~cast:(int_of_string_opt, string_of_int) ~type_:"int"

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
  let merge_to_predicate name to_predicate1 to_predicate2 =
    match to_predicate1, to_predicate2 with
    | Nullary formula1, Nullary formula2 ->
      Some (Nullary (Formula.or_ formula1 formula2))
    | Unary to_formula1, Unary to_formula2 ->
      Some (Unary (fun text_formula -> formula_result_or_ (to_formula1 text_formula) (to_formula2 text_formula)))
    | _ ->
      kspf invalid_arg "TextFormula.Converter.merge: predicate \"%s\" appears in both with different arities" name
  in
  {
    raw = (fun string -> formula_result_or_ (converter1.raw string) (converter2.raw string));
    to_predicates = String.Map.union merge_to_predicate converter1.to_predicates converter2.to_predicates;
  }

let merge_l = function
  | [] -> invalid_arg "TextFormula.Converter.merge_l"
  | converter :: converters -> List.fold_left merge converter converters
