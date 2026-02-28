open Nes
open Formula
module Type = Text_formula_type
module Printer = Text_formula_printer

type 'p case = {
  text_predicate_to_formula: Type.predicate -> ('p Formula.t, string) Result.t option; (** Given a text formula predicate, produce either a ['p Formula.t] or an [Error] if the case matches, or [None]. *)
  predicate_to_text_formula: 'p -> Type.t option; (** Given a ['p]redicate, produce a text formula if the case matches, or [None]. *)
}

type tiebreaker = Left | Right | Both

type _ t =
  | Cases : 'p case list -> 'p t
  | Map : (('p Formula.t -> 'q) * (string -> string) * 'p t) -> 'q t
  | Merge : tiebreaker * 'p t * 'p t -> 'p t

let make ~raw cs =
  let raw_case = {
    text_predicate_to_formula = (function
      | Type.Raw s -> Some (raw s)
      | _ -> None
    );
    predicate_to_text_formula = const None;
  }
  in
  Cases (raw_case :: cs)

let map ?(error = Fun.id) f c = Map (f, error, c)

let merge ?(tiebreaker = Both) c1 c2 = Merge (tiebreaker, c1, c2)

let merge_l = function
  | [] -> invalid_arg "Text_formula_converter.merge_l"
  | [c] -> c
  | c :: cs -> List.fold_left merge c cs

let text_predicate_to_formula c tp =
  let rec text_predicate_to_formula : type p. p t -> (p Formula.t, string) Result.t = function
    | Cases cases ->
      Option.value
        ~default: (kaspf error "No converter for predicate: %a." Printer.pp_predicate tp)
        (List.map_first_some (fun case -> case.text_predicate_to_formula tp) cases)
    | Map (f, error, c) -> Result.map_both ~ok: (Formula.pred % f) ~error @@ text_predicate_to_formula c
    | Merge (tiebreaker, c1, c2) ->
      match (text_predicate_to_formula c1, text_predicate_to_formula c2) with
      | Ok f1, Ok f2 when tiebreaker = Both -> Ok (Formula.or_ f1 f2)
      | _, Ok f2 when tiebreaker = Right -> Ok f2
      | Ok f1, _ -> Ok f1
      | _, Ok f2 -> Ok f2
      | Error e1, Error e2 -> Error (e1 ^ "\n" ^ e2)
  in
  text_predicate_to_formula c

let text_formula_to_formula converter = Formula.convert_res (text_predicate_to_formula converter)

let formula_to_text_formula converter formula =
  let rec predicate_to_text_formula : type p. p t -> p -> Type.t option = fun c p ->
    match c with
    | Cases cases -> List.map_first_some (fun case -> case.predicate_to_text_formula p) cases
    | Map (_, _, _) -> None
    | Merge (tiebreaker, c1, c2) ->
      match (predicate_to_text_formula c1 p, predicate_to_text_formula c2 p) with
      | Some f1, Some f2 when tiebreaker = Both -> Some (Formula.or_ f1 f2)
      | _, Some f2 when tiebreaker = Right -> Some f2
      | Some f1, _ -> Some f1
      | _, Some f2 -> Some f2
      | None, None -> None
  in
  match Formula.convert_opt (predicate_to_text_formula converter) formula with
  | None -> failwith "Text_formula_converter.of_formula: incomplete formula converter"
  | Some tf -> tf

let nullary ~name p = {
  text_predicate_to_formula = (function
    | Type.Nullary name' when name' = name -> Some (Ok (Formula.pred p))
    | _ -> None
  );
  predicate_to_text_formula = (fun p' ->
    (* FIXME: predicate equality *)
    match p = p' with
    | true -> Some (Type.nullary' name)
    | false -> None
  );
}

let unary ~name f predicate_to_text_formula = {
  text_predicate_to_formula = (function
    | Type.Unary (name', tp) when name = name' -> Some (Result.map Formula.pred (f tp))
    | _ -> None
  );
  predicate_to_text_formula;
}

type wrap_back = Always | Never | Not_pred | Not_raw | Custom of (Type.t -> Type.t)

let apply_wrap_back ~name = function
  | Always -> Type.unary' name
  | Never -> Fun.id
  | Not_pred -> (function Pred p -> Pred p | f -> Type.unary' name f)
  | Not_raw -> (function Pred (Raw _) as f -> f | f -> Type.unary' name f)
  | Custom c -> c

let unary_raw ?(wrap_back = Always) ~name ~cast: (cast, uncast) ~type_ (to_predicate, from_predicate) =
  unary
    ~name
    (function
      | Pred (Raw s) ->
        Result.map
          to_predicate
          (
            Option.to_result
              ~none: (spf "the unary predicate \"%s:\" only accepts %s arguments." name type_)
              (cast s)
          )
      | _ ->
        Error (spf "the unary predicate \"%s:\" only accepts a %s arguments." name type_)
    )
    (Option.map (apply_wrap_back ~name wrap_back % Type.raw' % uncast) % from_predicate)

let unary_string = unary_raw ~cast: (some, Fun.id) ~type_: "string"
let unary_int = unary_raw ~cast: (int_of_string_opt, string_of_int) ~type_: "int"

let unary_id = unary_raw ~cast: (Entry.Id.of_string, Entry.Id.to_string) ~type_: "id"

let unary_lift ?(wrap_back = Always) ~name ~converter (lift, unlift) =
  unary
    ~name
    (Result.map lift % text_formula_to_formula converter)
    (Option.map (apply_wrap_back ~name wrap_back % formula_to_text_formula converter) % unlift)
