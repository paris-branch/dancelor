open Nes
open Formula
module Type = TextFormulaType

type 'p case = {
  to_: (Type.predicate, ('p Formula.t, string) Result.t) Link.t;
  from: ('p, Type.predicate) Link.t;
}

let to_ case = case.to_
(* let from case = case.from *)

type _ t =
  | Cases : 'p case list -> 'p t
  | Map : (('p Formula.t -> 'q) * ('q -> 'p Formula.t option) * 'p t) -> 'q t
  | Merge : 'p t * 'p t -> 'p t

let make cs = Cases cs
let map (f, g) c = Map (f, g, c)
let merge c1 c2 = Merge (c1, c2)

let merge_l = function
  | [] -> invalid_arg "TextFormulaConverter.merge_l"
  | [c] -> c
  | c :: cs -> List.fold_left merge c cs

let predicate_to_formula c tp =
  let rec aux : type p. p t -> (p Formula.t, string) Result.t = function
    | Cases cases -> Link.link' ~default:(Error "no converter for this predicate") (List.map to_ cases) tp
    | Map (f, _, c) -> Result.map (Formula.pred % f) (aux c)
    | Merge (c1, c2) ->
      match (aux c1, aux c2) with
      | Ok f1, Ok f2 -> Ok (Formula.or_ f1 f2)
      | Ok f1, _ -> Ok f1
      | _, Ok f2 -> Ok f2
      | Error e1, Error e2 -> Error (e1 ^ "\n" ^ e2)
  in
  aux c

let to_formula converter = Formula.convert (predicate_to_formula converter)

let raw f =
  let to_ = function
    | Type.Raw s -> Some (f s)
    | _ -> None
  in
  let from = Fun.const None in
  {to_; from}

let nullary ~name p =
  let to_ = function
    | Type.Nullary name' when name' = name -> Some (Ok (Formula.pred p))
    | _ -> None
  in
  let from = Fun.const None in
  {to_; from}

let unary ~name f =
  let to_ = function
    | Type.Unary (name', tp) when name = name' -> Some (Result.map Formula.pred (f tp))
    | _ -> None
  in
  let from = Fun.const None in
  {to_; from}

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

let unary_lift ~name ~converter (lift, _unlift) =
  unary ~name (Result.map lift % to_formula converter)
