open Nes
open Formula
module Type = Text_formula_type
module Printer = Text_formula_printer

type 'p case = {
  text_predicate_to_formula: Type.predicate -> ('p Formula.t, string) Result.t option; (** Given a text formula predicate, produce either a ['p Formula.t] or an [Error] if the case matches, or [None]. *)
  predicate_to_text_formula: 'p -> Type.t option; (** Given a ['p]redicate, produce a text formula if the case matches, or [None]. *)
}

type 'p t = {
  debug_name: string;
  debug_print: Format.formatter -> 'p -> unit;
  raw: string -> ('p Formula.t, string) result; (** Given a string outside predicates, produce either a ['p Formula.t] or an [Error]. *)
  other_cases: 'p case list;
}
[@@deriving fields]

let make ~debug_name ~debug_print ~raw other_cases =
  {debug_name; debug_print; raw; other_cases}

let cases converter =
  let raw_case = {
    text_predicate_to_formula = (function Type.Raw s -> Some (converter.raw s) | _ -> None);
    predicate_to_text_formula = const None;
  }
  in
  raw_case :: converter.other_cases

let text_predicate_to_formula converter text_predicate =
  Option.value
    ~default: (kaspf error "No “%s” converter for predicate: %a." converter.debug_name Printer.pp_predicate text_predicate)
    (List.find_map (fun case -> case.text_predicate_to_formula text_predicate) (cases converter))

let text_formula_to_formula converter = Formula.convert_res (text_predicate_to_formula converter)

let predicate_to_text_formula converter predicate =
  List.find_map (fun case -> case.predicate_to_text_formula predicate) (cases converter)

let formula_to_text_formula converter formula =
  match Formula.convert_opt (predicate_to_text_formula converter) formula with
  | None -> failwith @@ spf "Text_formula_converter.of_formula: incomplete formula converter “%s”" converter.debug_name
  | Some tf -> tf

let map ?(error = Fun.id) f converter = {
  debug_name = "<opaque map>";
  debug_print = (fun fmt _ -> fpf fmt "<opaque map>");
  raw = Result.map_both ~ok: (Formula.pred % f) ~error % converter.raw;
  other_cases =
  List.map
    (fun case ->
      {
        text_predicate_to_formula = Option.map (Result.map_both ~ok: (Formula.pred % f) ~error) % case.text_predicate_to_formula;
        predicate_to_text_formula = const None;
      }
    )
    converter.other_cases;
}

type tiebreaker = Left | Right | Both

let merge ?(tiebreaker = Both) converter1 converter2 =
  let merge_results r1 r2 =
    match (r1, r2) with
    | Ok f1, Ok f2 when tiebreaker = Both -> Ok (Formula.or_ f1 f2)
    | _, Ok f2 when tiebreaker = Right -> Ok f2
    | Ok f1, _ -> Ok f1
    | _, Ok f2 -> Ok f2
    | Error e1, Error e2 -> Error (e1 ^ "\n" ^ e2)
  in
  let merge_options o1 o2 = Result.to_option (merge_results (Option.to_result ~none: "" o1) (Option.to_result ~none: "" o2)) in
  {
    debug_name = spf "%s+%s" converter1.debug_name converter2.debug_name;
    debug_print = (fun fmt _ -> fpf fmt "<opaque merge>");
    raw = (fun str -> merge_results (converter1.raw str) (converter2.raw str));
    other_cases = [
      {
        text_predicate_to_formula = (fun text_predicate ->
          Some (
            merge_results
              (text_predicate_to_formula converter1 text_predicate)
              (text_predicate_to_formula converter2 text_predicate)
          )
        );
        predicate_to_text_formula = (fun predicate ->
          merge_options
            (predicate_to_text_formula converter1 predicate)
            (predicate_to_text_formula converter2 predicate)
        );
      }
    ];
  }

let merge_l = function
  | [] -> invalid_arg "Text_formula_converter.merge_l"
  | c :: cs -> List.fold_left merge c cs

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
