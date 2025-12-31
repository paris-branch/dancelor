open Nes

type predicate =
  | Is of ModelBuilder.Core.Book.t Entry.Id.t
  | Title of string
  | TitleMatches of string
  | ExistsVersion of Version.t
  | ExistsSet of Set.t
  | ExistsVersionDeep of Version.t
  | ExistsEditor of Person.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let titlematches' = Formula.pred % titlematches
let existsversion' = Formula.pred % existsversion
let existsset' = Formula.pred % existsset
let existsversiondeep' = Formula.pred % existsversiondeep
let existseditor' = Formula.pred % existseditor

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (ok % titlematches');
        unary_string ~name: "title" (title, title_val);
        unary_string ~name: "title-matches" (titlematches, titlematches_val);
        unary_lift ~name: "exists-version" (existsversion, existsversion_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "exists-set" (existsset, existsset_val) ~converter: Set.text_formula_converter;
        unary_lift ~name: "exists-version-deep" (existsversiondeep, existsversiondeep_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "exists-editor" (existseditor, existseditor_val) ~converter: Person.text_formula_converter;
        unary_id ~name: "is" (is, is_val);
      ]
  )

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is x = is @@ Entry.id x
let is' x = Formula.pred @@ is x

let memversion x = existsversion @@ Version.is' x
let memset x = existsset @@ Set.is' x
let memversiondeep x = existsversiondeep @@ Version.is' x
let existstunedeep x = existsversiondeep @@ Version.tune' x
let memtunedeep x = existstunedeep @@ Tune.is' x
let memeditor x = existseditor @@ Person.is' x

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
    | (ExistsVersion f1, ExistsVersion f2) -> some @@ existsversion (op f1 f2)
    | (ExistsSet f1, ExistsSet f2) -> some @@ existsset (op f1 f2)
    | (ExistsVersionDeep f1, ExistsVersionDeep f2) -> some @@ existsversiondeep (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p)
      | (Title _ as p)
      | (TitleMatches _ as p) ->
        p
      | ExistsVersion vfilter -> existsversion @@ Version.optimise vfilter
      | ExistsSet sfilter -> existsset @@ Set.optimise sfilter
      | ExistsVersionDeep vfilter -> existsversiondeep @@ Version.optimise vfilter
      | ExistsEditor pfilter -> existseditor @@ Person.optimise pfilter
    )
