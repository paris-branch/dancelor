open Nes

type predicate =
  | Is of Model_builder.Core.Book.t Entry.Id.t
  | Title of string
  | Title_matches of string
  | Exists_version of Version.t
  | Exists_set of Set.t
  | Exists_version_deep of Version.t
  | Exists_editor of Person.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let title_matches' = Formula.pred % title_matches
let exists_version' = Formula.pred % exists_version
let exists_set' = Formula.pred % exists_set
let exists_version_deep' = Formula.pred % exists_version_deep
let exists_editor' = Formula.pred % exists_editor

let text_formula_converter =
  Text_formula_converter.(
    make
      [
        raw (ok % title_matches');
        unary_string ~name: "title" (title, title_val);
        unary_string ~name: "title-matches" (title_matches, title_matches_val);
        unary_lift ~name: "exists-version" (exists_version, exists_version_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "exists-set" (exists_set, exists_set_val) ~converter: Set.text_formula_converter;
        unary_lift ~name: "exists-version-deep" (exists_version_deep, exists_version_deep_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "exists-editor" (exists_editor, exists_editor_val) ~converter: Person.text_formula_converter;
        unary_id ~name: "is" (is, is_val);
      ]
  )

let from_text_formula = Text_formula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (Text_formula.from_string ?filename input) from_text_formula

let to_text_formula = Text_formula.of_formula text_formula_converter
let to_string = Text_formula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let memversion x = exists_version @@ Version.is' x
let memset x = exists_set @@ Set.is' x
let memversiondeep x = exists_version_deep @@ Version.is' x
let existstunedeep x = exists_version_deep @@ Version.tune' x
let memtunedeep x = existstunedeep @@ Tune.is' x
let memeditor x = exists_editor @@ Person.is' x

let memversion' x = Formula.pred @@ memversion x
let memset' x = Formula.pred @@ memset x
let memversiondeep' x = Formula.pred @@ memversiondeep x
let existstunedeep' x = Formula.pred @@ existstunedeep x
let memtunedeep' x = Formula.pred @@ memtunedeep x
let memeditor' x = Formula.pred @@ memeditor x

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Exists_version f1, Exists_version f2) -> some @@ exists_version (op f1 f2)
    | (Exists_set f1, Exists_set f2) -> some @@ exists_set (op f1 f2)
    | (Exists_version_deep f1, Exists_version_deep f2) -> some @@ exists_version_deep (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p)
      | (Title _ as p)
      | (Title_matches _ as p) ->
        p
      | Exists_version vfilter -> exists_version @@ Version.optimise vfilter
      | Exists_set sfilter -> exists_set @@ Set.optimise sfilter
      | Exists_version_deep vfilter -> exists_version_deep @@ Version.optimise vfilter
      | Exists_editor pfilter -> exists_editor @@ Person.optimise pfilter
    )
