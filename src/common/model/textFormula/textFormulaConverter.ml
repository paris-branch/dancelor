open Nes
open Formula
module Type = TextFormulaType
module Printer = TextFormulaPrinter

type 'p case = {
  to_: (Type.predicate, ('p Formula.t, string) Result.t) Link.t;
  from: ('p, Type.t) Link.t;
}

let to_ case = case.to_
let from case = case.from

type tiebreaker = Left | Right | Both

type _ t =
  | Cases : 'p case list -> 'p t
  | Map : (('p Formula.t -> 'q) * (string -> string) * 'p t) -> 'q t
  | Merge : tiebreaker * 'p t * 'p t -> 'p t

let make cs = Cases cs
let map ?(error=Fun.id) f c = Map (f, error, c)
let merge ?(tiebreaker=Both) c1 c2 = Merge (tiebreaker, c1, c2)

let merge_l = function
  | [] -> invalid_arg "TextFormulaConverter.merge_l"
  | [c] -> c
  | c :: cs -> List.fold_left merge c cs

let predicate_to_formula c tp =
  let rec aux : type p. p t -> (p Formula.t, string) Result.t = function
    | Cases cases -> Link.link' ~default:(kaspf Result.error "No converter for predicate: %a." Printer.pp_predicate tp) (List.map to_ cases) tp
    | Map (f, error, c) -> Result.map_error error @@ Result.map (Formula.pred % f) @@ aux c
    | Merge (tiebreaker, c1, c2) ->
      match (aux c1, aux c2) with
      | Ok f1, Ok f2 when tiebreaker = Both -> Ok (Formula.or_ f1 f2)
      | _, Ok f2 when tiebreaker = Right -> Ok f2
      | Ok f1, _ -> Ok f1
      | _, Ok f2 -> Ok f2
      | Error e1, Error e2 -> Error (e1 ^ "\n" ^ e2)
  in
  aux c

let to_formula converter = Formula.convert_res (predicate_to_formula converter)

let of_formula converter formula =
  let rec aux : type p. p t -> p -> Type.t option = fun c p ->
    match c with
    | Cases cases -> Link.link (List.map from cases) p
    | Map (_, _, _) -> None
    | Merge (tiebreaker, c1, c2) ->
      match (aux c1 p, aux c2 p) with
      | Some f1, Some f2 when tiebreaker = Both -> Some (Formula.or_ f1 f2)
      | _, Some f2 when tiebreaker = Right -> Some f2
      | Some f1, _ -> Some f1
      | _, Some f2 -> Some f2
      | None, None -> None
  and aux' : type p. p t -> p Formula.t -> Type.t option = fun c f ->
    Formula.convert_opt (aux c) f
  in
  match aux' converter formula with
  | None -> failwith "TextFormulaConverter.of_formula: incomplete formula converter"
  | Some tf -> tf

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
  (* FIXME: predicate equality *)
  let from p' = match p = p' with
    | true -> Some (Type.nullary' name)
    | false -> None
  in
  {to_; from}

let unary ~name f from =
  let to_ = function
    | Type.Unary (name', tp) when name = name' -> Some (Result.map Formula.pred (f tp))
    | _ -> None
  in
  {to_; from}

type wrap_back = Always | Never | NotPred | NotRaw | Custom of (Type.t -> Type.t)

let apply_wrap_back ~name = function
  | Always -> Type.unary' name
  | Never -> Fun.id
  | NotPred -> (function Pred p -> Pred p | f -> Type.unary' name f)
  | NotRaw -> (function Pred (Raw _) as f -> f | f -> Type.unary' name f)
  | Custom c -> c

let unary_raw ?(wrap_back=Always) ~name ~cast:(cast, uncast) ~type_ (to_predicate, from_predicate) =
  unary ~name
    (function
      | Pred (Raw s) ->
        Result.map to_predicate
          (Option.to_result
             ~none: (spf "the unary predicate \"%s:\" only accepts %s arguments." name type_)
             (cast s))
      | _ ->
        Error (spf "the unary predicate \"%s:\" only accepts a %s arguments." name type_))
    (Option.map (apply_wrap_back ~name wrap_back % Type.raw' % uncast) % from_predicate)

let unary_string = unary_raw ~cast:(Option.some, Fun.id) ~type_:"string"
let unary_int = unary_raw ~cast:(int_of_string_opt, string_of_int) ~type_:"int"

let unary_lift ?(wrap_back=Always) ~name ~converter (lift, unlift) =
  unary ~name
    (Result.map lift % to_formula converter)
    (Option.map (apply_wrap_back ~name wrap_back % of_formula converter) % unlift)
