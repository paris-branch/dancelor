open Nes

let _key = "any-filter"

type predicate =
  | Type of AnyCore.Type.t
  (* lifting predicates: *)
  | AsCredit  of  CreditFilter.t
  | AsDance   of   DanceFilter.t
  | AsPerson  of  PersonFilter.t
  | AsBook    of    BookFilter.t
  | AsSet     of     SetFilter.t
  | AsTune    of    TuneFilter.t
  | AsVersion of VersionFilter.t
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let type_ type_ = Formula.pred (Type type_)

let asCredit  filter = Formula.pred (AsCredit  filter)
let asDance   filter = Formula.pred (AsDance   filter)
let asPerson  filter = Formula.pred (AsPerson  filter)
let asBook    filter = Formula.pred (AsBook    filter)
let asSet     filter = Formula.pred (AsSet     filter)
let asTune    filter = Formula.pred (AsTune    filter)
let asVersion filter = Formula.pred (AsVersion filter)

let raw str =
  let filters = [
    (match  CreditFilter.raw str with Ok cfilter -> Ok ( asCredit cfilter) | Error err -> error_fmt " credit: %s" err);
    (match   DanceFilter.raw str with Ok dfilter -> Ok (  asDance dfilter) | Error err -> error_fmt "  dance: %s" err);
    (match  PersonFilter.raw str with Ok pfilter -> Ok ( asPerson pfilter) | Error err -> error_fmt " person: %s" err);
    (match    BookFilter.raw str with Ok bfilter -> Ok (   asBook bfilter) | Error err -> error_fmt "   book: %s" err);
    (match     SetFilter.raw str with Ok sfilter -> Ok (    asSet sfilter) | Error err -> error_fmt "    set: %s" err);
    (match    TuneFilter.raw str with Ok tfilter -> Ok (   asTune tfilter) | Error err -> error_fmt "   tune: %s" err);
    (match VersionFilter.raw str with Ok vfilter -> Ok (asVersion vfilter) | Error err -> error_fmt "version: %s" err);
  ] in
  let filters, errors =
    List.partition_map
      (function Ok filter -> Left filter | Error err -> Right err)
      filters
  in
  if filters <> [] then
    Ok (Formula.or_l filters)
  else
    match errors with
    | [] -> assert false
    | err :: _ -> Error err (* FIXME: also show the others *)

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "type", raw_only ~convert:(fun s ->
        match AnyCore.Type.of_string_opt s with
        | Some t -> Ok t
        | None -> error_fmt ("There is an error in your request: "
                             ^^ "\"%s\" is not a valid type.") s) type_;
  ]

let from_text_formula =
  let from_text_predicate pred =
    let filters = [
      TextFormula.make_predicate_to_formula raw
        nullary_text_predicates unary_text_predicates pred;
      (match  CreditFilter.from_text_formula (Pred pred) with Ok cfilter -> Ok  (asCredit cfilter) | Error err -> error_fmt " credit: %s" err);
      (match   DanceFilter.from_text_formula (Pred pred) with Ok dfilter -> Ok   (asDance dfilter) | Error err -> error_fmt "  dance: %s" err);
      (match  PersonFilter.from_text_formula (Pred pred) with Ok pfilter -> Ok  (asPerson pfilter) | Error err -> error_fmt " person: %s" err);
      (match    BookFilter.from_text_formula (Pred pred) with Ok bfilter -> Ok    (asBook bfilter) | Error err -> error_fmt "   book: %s" err);
      (match     SetFilter.from_text_formula (Pred pred) with Ok sfilter -> Ok     (asSet sfilter) | Error err -> error_fmt "    set: %s" err);
      (match    TuneFilter.from_text_formula (Pred pred) with Ok tfilter -> Ok    (asTune tfilter) | Error err -> error_fmt "   tune: %s" err);
      (match VersionFilter.from_text_formula (Pred pred) with Ok vfilter -> Ok (asVersion vfilter) | Error err -> error_fmt "version: %s" err);
    ] in
    let filters, errors =
      List.partition_map
        (function Ok filter -> Left filter | Error err -> Right err)
        filters
    in
    if filters <> [] then
      Ok (Formula.or_l filters)
    else
      match errors with
      | [] -> assert false
      | _ ->
        error_fmt
          ("There is a part of your formula on which all types encountered an error.\n"
           ^^ "The part in question is: %a.\n"
           ^^ "The errors are: %s")
          (TextFormula.Printer.pp_predicate False) pred
          (String.concat "; " errors) (* FIXME: list of errors (one per line?) *)
  in
  TextFormula.to_formula from_text_predicate

(** All the possible types that a formula can return. *)
let rec possible_types =
  let open Formula in
  function
  | False -> AnyCore.Type.Set.empty
  | True -> AnyCore.Type.all
  | Not formula -> AnyCore.Type.Set.diff AnyCore.Type.all (possible_types formula)
  | And (formula1, formula2) -> AnyCore.Type.Set.inter (possible_types formula1) (possible_types formula2)
  | Or  (formula1, formula2) -> AnyCore.Type.Set.union (possible_types formula1) (possible_types formula2)
  | Pred pred ->
    (* FIXME: We could do better here by checking in depth whether a formula
       has a chance to return. That would eliminate some other types. *)
    match pred with
    | Type type_  -> AnyCore.Type.Set.singleton type_
    | AsCredit  _ -> AnyCore.Type.Set.singleton Credit
    | AsDance   _ -> AnyCore.Type.Set.singleton Dance
    | AsPerson  _ -> AnyCore.Type.Set.singleton Person
    | AsBook    _ -> AnyCore.Type.Set.singleton Book
    | AsSet     _ -> AnyCore.Type.Set.singleton Set
    | AsTune    _ -> AnyCore.Type.Set.singleton Tune
    | AsVersion _ -> AnyCore.Type.Set.singleton Version

let nullary_text_predicates_of_type type_ =
  String.Set.of_list
    (match type_ with
     | AnyCore.Type.Credit  -> List.map fst  CreditFilter.nullary_text_predicates
     | AnyCore.Type.Dance   -> List.map fst   DanceFilter.nullary_text_predicates
     | AnyCore.Type.Person  -> List.map fst  PersonFilter.nullary_text_predicates
     | AnyCore.Type.Book    -> List.map fst    BookFilter.nullary_text_predicates
     | AnyCore.Type.Set     -> List.map fst     SetFilter.nullary_text_predicates
     | AnyCore.Type.Tune    -> List.map fst    TuneFilter.nullary_text_predicates
     | AnyCore.Type.Version -> List.map fst VersionFilter.nullary_text_predicates)

let nullary_text_predicates_of_types types =
  AnyCore.Type.Set.fold
    (String.Set.union @@@ nullary_text_predicates_of_type)
    types (String.Set.of_list (List.map fst nullary_text_predicates))

let unary_text_predicates_of_type type_ =
  String.Set.of_list
    (match type_ with
     | AnyCore.Type.Credit  -> List.map fst  CreditFilter.unary_text_predicates
     | AnyCore.Type.Dance   -> List.map fst   DanceFilter.unary_text_predicates
     | AnyCore.Type.Person  -> List.map fst  PersonFilter.unary_text_predicates
     | AnyCore.Type.Book    -> List.map fst    BookFilter.unary_text_predicates
     | AnyCore.Type.Set     -> List.map fst     SetFilter.unary_text_predicates
     | AnyCore.Type.Tune    -> List.map fst    TuneFilter.unary_text_predicates
     | AnyCore.Type.Version -> List.map fst VersionFilter.unary_text_predicates)

let unary_text_predicates_of_types types =
  AnyCore.Type.Set.fold
    (String.Set.union @@@ unary_text_predicates_of_type)
    types (String.Set.of_list (List.map fst unary_text_predicates))

(* let check_predicates text_formula = *)

exception UnknownPredicate of string * string

let from_string string =
  try
    (
      let text_formula = TextFormula.from_string string in
      match from_text_formula text_formula with (* can yield parse error *)
      | Ok formula ->
        (
          let types_ = possible_types formula in
          (
            let unknown_nullary_text_predicates =
              String.Set.diff
                (TextFormula.nullary_predicates text_formula)
                (nullary_text_predicates_of_types types_)
            in
            match String.Set.choose_opt unknown_nullary_text_predicates with
            | None -> ()
            | Some pred -> raise (UnknownPredicate ("nullary", pred))
          );
          (
            let unknown_unary_text_predicates =
              String.Set.diff
                (TextFormula.unary_predicates text_formula)
                (unary_text_predicates_of_types types_)
            in
            match String.Set.choose_opt unknown_unary_text_predicates with
            | None -> ()
            | Some pred -> raise (UnknownPredicate ("unary", pred))
          );
          Ok formula
        )
      | Error err -> Error err
    )
  with
  | TextFormula.Lexer.UnexpectedCharacter char ->
    error_fmt ("There is an unexpected character in your request: '%c'. "
               ^^ "If you really want to type it, protect it with quotes, "
               ^^ "eg. \"foo%cbar\".") char char
  | TextFormula.Lexer.UnterminatedQuote ->
    error_fmt ("There is an unterminated quote in your request. "
               ^^ "If you just want to type a quote character, "
               ^^ "whether inside quotes or not, escape it, eg. \"foo\\\"bar\".")
  | TextFormula.Parser.ParseError (_, _, where) ->
    error_fmt "There is a syntax error %s in your request." where
  | UnknownPredicate(arity, pred) ->
    error_fmt "There is an unknown %s predicate in your request: \"%s\"." arity pred
  | exn ->
    error_fmt ("Handling your request caused an unknown exception: %s. "
               ^^ "Contact your system administrator with this message.")
      (Printexc.to_string exn)
