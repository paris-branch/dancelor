open Nes

type predicate =
  | Is of ModelBuilder.Core.Book.t Entry.Id.t
  | IsSource
  | Title of string
  | TitleMatches of string
  | Subtitle of string
  | SubtitleMatches of string
  | ExistsVersion of Version.t
  | ExistsSet of Set.t
  | ExistsInlineSet of Set.t
  | ExistsVersionDeep of Version.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let titleMatches' = Formula.pred % titleMatches
let subtitle' = Formula.pred % subtitle
let subtitleMatches' = Formula.pred % subtitleMatches
let isSource' = Formula.pred IsSource
let existsVersion' = Formula.pred % existsVersion
let existsSet' = Formula.pred % existsSet
let existsInlineSet' = Formula.pred % existsInlineSet
(* let existsVersionDeep' = Formula.pred % existsVersionDeep *)

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (fun string -> Ok (Formula.or_ (titleMatches' string) (subtitleMatches' string)));
        unary_string ~name: "title" (title, unTitle);
        unary_string ~name: "title-matches" (titleMatches, unTitleMatches);
        unary_string ~name: "subtitle" (subtitle, unSubtitle);
        unary_string ~name: "subtitle-matches" (subtitleMatches, unSubtitleMatches);
        unary_lift ~name: "exists-version" (existsVersion, unExistsVersion) ~converter: Version.text_formula_converter;
        unary_lift ~name: "exists-set" (existsSet, unExistsSet) ~converter: Set.text_formula_converter;
        unary_lift ~name: "exists-inline-set" (existsInlineSet, unExistsInlineSet) ~converter: Set.text_formula_converter;
        unary_lift ~name: "exists-version-deep" (existsVersionDeep, unExistsVersionDeep) ~converter: Version.text_formula_converter;
        unary_id ~name: "is" (is, unIs);
        nullary ~name: "is-source" IsSource;
      ]
  )

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind (TextFormula.from_string ?filename input) from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is = is % Entry.id
let is' = Formula.pred % is

let memVersion = existsVersion % Version.is'
let memSet = existsSet % Set.is'
let memVersionDeep = existsVersionDeep % Version.is'
let existsTuneDeep = existsVersionDeep % Version.tune'
let memTuneDeep = existsTuneDeep % Tune.is'

let memVersion' = Formula.pred % memVersion
let memSet' = Formula.pred % memSet
let existsVersionDeep' = Formula.pred % existsVersionDeep
let memVersionDeep' = Formula.pred % memVersionDeep
let existsTuneDeep' = Formula.pred % existsTuneDeep
let memTuneDeep' = Formula.pred % memTuneDeep

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (ExistsVersion f1, ExistsVersion f2) -> some @@ existsVersion (op f1 f2)
    | (ExistsSet f1, ExistsSet f2) -> some @@ existsSet (op f1 f2)
    | (ExistsInlineSet f1, ExistsInlineSet f2) -> some @@ existsInlineSet (op f1 f2)
    | (ExistsVersionDeep f1, ExistsVersionDeep f2) -> some @@ existsVersionDeep (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p)
      | (Title _ as p)
      | (TitleMatches _ as p)
      | (Subtitle _ as p)
      | (SubtitleMatches _ as p)
      | (IsSource as p) ->
        p
      | ExistsVersion vfilter -> existsVersion @@ Version.optimise vfilter
      | ExistsSet sfilter -> existsSet @@ Set.optimise sfilter
      | ExistsInlineSet sfilter -> existsInlineSet @@ Set.optimise sfilter
      | ExistsVersionDeep vfilter -> existsVersionDeep @@ Version.optimise vfilter
    )
