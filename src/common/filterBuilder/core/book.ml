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
  | ExistsVersionDeep of Version.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let titlematches' = Formula.pred % titlematches
let subtitle' = Formula.pred % subtitle
let subtitlematches' = Formula.pred % subtitlematches
let issource' = Formula.pred IsSource
let existsversion' = Formula.pred % existsversion
let existsset' = Formula.pred % existsset
(* let existsversiondeep' = Formula.pred % existsversiondeep *)

let text_formula_converter =
  TextFormulaConverter.(
    make
      [
        raw (fun string -> Ok (Formula.or_ (titlematches' string) (subtitlematches' string)));
        unary_string ~name: "title" (title, title_val);
        unary_string ~name: "title-matches" (titlematches, titlematches_val);
        unary_string ~name: "subtitle" (subtitle, subtitle_val);
        unary_string ~name: "subtitle-matches" (subtitlematches, subtitlematches_val);
        unary_lift ~name: "exists-version" (existsversion, existsversion_val) ~converter: Version.text_formula_converter;
        unary_lift ~name: "exists-set" (existsset, existsset_val) ~converter: Set.text_formula_converter;
        unary_lift ~name: "exists-version-deep" (existsversiondeep, existsversiondeep_val) ~converter: Version.text_formula_converter;
        unary_id ~name: "is" (is, is_val);
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

let memversion = existsversion % Version.is'
let memset = existsset % Set.is'
let memversiondeep = existsversiondeep % Version.is'
let existstunedeep = existsversiondeep % Version.tune'
let memtunedeep = existstunedeep % Tune.is'

let memversion' = Formula.pred % memversion
let memset' = Formula.pred % memset
let existsversiondeep' = Formula.pred % existsversiondeep
let memversiondeep' = Formula.pred % memversiondeep
let existstunedeep' = Formula.pred % existstunedeep
let memtunedeep' = Formula.pred % memtunedeep

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
      | (TitleMatches _ as p)
      | (Subtitle _ as p)
      | (SubtitleMatches _ as p)
      | (IsSource as p) ->
        p
      | ExistsVersion vfilter -> existsversion @@ Version.optimise vfilter
      | ExistsSet sfilter -> existsset @@ Set.optimise sfilter
      | ExistsVersionDeep vfilter -> existsversiondeep @@ Version.optimise vfilter
    )
