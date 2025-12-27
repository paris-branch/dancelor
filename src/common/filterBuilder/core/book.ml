open Nes

type predicate =
  | Is of ModelBuilder.Core.Book.t Entry.Id.t
  | Title of string
  | TitleMatches of string
  | ExistsVersion of Version.t
  | ExistsSet of Set.t
  | ExistsVersionDeep of Version.t
  | ExistsEditor of Person.t
[@@deriving eq, show {with_path = false}, biniou, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, biniou, yojson]

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

let is = is % Entry.id
let is' = Formula.pred % is

let memversion = existsversion % Version.is'
let memset = existsset % Set.is'
let memversiondeep = existsversiondeep % Version.is'
let existstunedeep = existsversiondeep % Version.tune'
let memtunedeep = existstunedeep % Tune.is'
let memeditor = existseditor % Person.is'

let memversion' = Formula.pred % memversion
let memset' = Formula.pred % memset
let memversiondeep' = Formula.pred % memversiondeep
let existstunedeep' = Formula.pred % existstunedeep
let memtunedeep' = Formula.pred % memtunedeep
let memeditor' = Formula.pred % memeditor

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
