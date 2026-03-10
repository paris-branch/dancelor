open Nes
open Formula
module Type = Text_formula_type
module Printer = Text_formula_printer

type 'p case = {
  name: string; (** A name for the case, used in error messages. *)
  text_predicate_to_formula: Type.predicate -> ('p Formula.t, string) Result.t option; (** Given a text formula predicate, produce either a ['p Formula.t] or an [Error] if the case matches, or [None]. *)
  predicate_to_text_formula: 'p -> Type.t option; (** Given a ['p]redicate, produce a text formula if the case matches, or [None]. *)
}

type inline = Inline | No_inline
(* FIXME: There probably needs to be variants handling Raw differently. *)

type _ lifter =
  Lifter :
    {
      name: string;
      lift: 'q Formula.t -> 'p;
      unlift: 'p -> 'q Formula.t option;
      converter: 'q t;
      inline: inline;
    } ->
      'p lifter

and 'p t = {
  debug_name: string;
  debug_print: Format.formatter -> 'p -> unit;
  raw: string -> ('p Formula.t, string) result; (** Given a string outside predicates, produce either a ['p Formula.t] or an [Error]. *)
  lifters: 'p lifter list;
  other_cases: 'p case list;
}

let raw converter = converter.raw
let debug_name converter = converter.debug_name
let debug_print converter = converter.debug_print

let lifter ~name ?(inline = No_inline) (lift, unlift) converter =
  Lifter {name; lift; unlift; converter; inline}

let make ~debug_name ~debug_print ~raw ?(lifters = []) other_cases =
  {debug_name; debug_print; raw; lifters; other_cases}

let rec cases
  : type p. p t -> p case list
= fun converter ->
  let raw_case = {
    name = "raw";
    text_predicate_to_formula = (function Type.Raw s -> Some (converter.raw s) | _ -> None);
    predicate_to_text_formula = const None;
  }
  in
  let lifter_cases =
    List.concat_map
      (fun (Lifter {name; lift; unlift; converter; inline}) ->
        {
          name = spf "%s lifter" name;
          text_predicate_to_formula = (function
            | Type.Unary (name', tp) when name = name' ->
              Some (Result.map Formula.pred ((Result.map lift % text_formula_to_formula converter) tp))
            | _ -> None
          );
          predicate_to_text_formula = (fun predicate ->
            match unlift predicate with
            | None -> None
            | Some qf ->
              let qf = formula_to_text_formula converter qf in
              match inline with
              | No_inline -> Some (Type.unary' name qf)
              | Inline -> Some qf
          );
        } ::
        match inline with
        | No_inline -> []
        | Inline ->
          List.map
            (fun case ->
              {
                name = spf "%s (lifted by %s)" case.name name;
                text_predicate_to_formula = Option.map (Result.map_both ~ok: (Formula.pred % lift) ~error: (spf "lifted %s: %s" name)) % case.text_predicate_to_formula;
                predicate_to_text_formula = const None;
              }
            )
            (cases converter)
      )
      converter.lifters
  in
  [raw_case] @ lifter_cases @ converter.other_cases

and text_predicate_to_formula : type p. p t -> Text_formula_type.predicate -> (p Formula.t, string) Result.t = fun converter text_predicate ->
  match List.filter_map (fun case -> case.text_predicate_to_formula text_predicate) (cases converter) with
  | [] -> Error (aspf "No “%s” converter for predicate: %a." converter.debug_name Printer.pp_predicate text_predicate)
  | results ->
    let (oks, errors) = List.partition_map (function Ok x -> Left x | Error y -> Right y) results in
    match oks with
    | [] -> Error (String.concat "\n" errors)
    | results -> Ok (Formula.or_l results)

and text_formula_to_formula : type p. p t -> Text_formula_type.t -> (p Formula.t, string) Result.t = fun converter ->
  Formula.convert_res (text_predicate_to_formula converter)

and predicate_to_text_formula : type p. p t -> p -> Text_formula_type.t option = fun converter predicate ->
  List.find_map (fun case -> case.predicate_to_text_formula predicate) (cases converter)

and formula_to_text_formula : type p. p t -> p Formula.t -> Text_formula_type.t = fun converter formula ->
  match Formula.convert_opt (predicate_to_text_formula converter) formula with
  | None -> failwith @@ spf "Text_formula_converter.of_formula: incomplete formula converter “%s”" converter.debug_name
  | Some tf -> tf

let map ?(error = Fun.id) f converter = {
  debug_name = "<opaque map>";
  debug_print = (fun fmt _ -> fpf fmt "<opaque map>");
  raw = Result.map_both ~ok: (Formula.pred % f) ~error % converter.raw;
  lifters =
  List.map
    (function
      | Lifter {name; lift; unlift = _; converter; inline} ->
        Lifter {
          name;
          lift = f % Formula.pred % lift;
          unlift = const None;
          converter;
          inline;
        }
    )
    converter.lifters;
  other_cases =
  List.map
    (fun case ->
      {
        name = case.name;
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
    raw = (fun _str -> Error "there is no raw in a merge");
    lifters = [];
    other_cases = [
      {
        name = spf "merge of %s and %s" converter1.debug_name converter2.debug_name;
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
  name;
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
  name;
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
