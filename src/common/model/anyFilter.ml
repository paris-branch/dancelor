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
  (* | AsSource of SourceFilter.t *)
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
  Formula.or_l [
    asCredit   (CreditFilter.raw str);
    asDance     (DanceFilter.raw str);
    asPerson   (PersonFilter.raw str);
    asBook       (BookFilter.raw str);
    asSet         (SetFilter.raw str);
    asTune       (TuneFilter.raw str);
    asVersion (VersionFilter.raw str);
  ]

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "type", raw_only ~convert:AnyCore.Type.of_string type_;
  ]

let from_text_formula =
  let from_text_predicate pred =
    Formula.or_l [
      TextFormula.make_predicate_to_formula raw
        nullary_text_predicates unary_text_predicates pred;
      asCredit   (CreditFilter.from_text_formula (Pred pred));
      asDance     (DanceFilter.from_text_formula (Pred pred));
      asPerson   (PersonFilter.from_text_formula (Pred pred));
      asBook       (BookFilter.from_text_formula (Pred pred));
      asSet         (SetFilter.from_text_formula (Pred pred));
      asTune       (TuneFilter.from_text_formula (Pred pred));
      asVersion (VersionFilter.from_text_formula (Pred pred));
    ]
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
  let text_formula = TextFormula.from_string string in
  let formula = from_text_formula text_formula in (* can raise parse error *)
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
  formula
