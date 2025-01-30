open Nes

type predicate =
  | Is of Core.Version.t Slug.t
  | Tune of Tune.t
  | Key of Music.key
  | Kind of Kind.Version.Filter.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let tune' = Formula.pred % tune
let key' = Formula.pred % key
let kind' = Formula.pred % kind

let text_formula_converter =
  TextFormulaConverter.(
    merge
      ~tiebreaker: Left
      (
        (* Version-specific converter. *)
        make
          [
            unary_lift ~wrap_back: NotRaw ~name: "tune" (tune, unTune) ~converter: Tune.text_formula_converter;
            unary_raw ~name: "key" (key, unKey) ~cast: (Music.key_of_string_opt, Music.key_to_string) ~type_: "key";
            unary_lift ~name: "kind" (kind, unKind) ~converter: Kind.Version.Filter.text_formula_converter;
            unary_string ~name: "is" (is % Slug.unsafe_of_string, Option.map Slug.to_string % unIs);
          ]
      )
      (
        (* Tune converter, lifted to versions. Lose in case of tiebreak. *)
        map tune Tune.text_formula_converter ~error: ((^) "As tune lifted to version: ")
      )
  )

let from_text_formula = TextFormula.to_formula text_formula_converter
let from_string ?filename input =
  Result.bind
    (TextFormula.from_string ?filename input)
    from_text_formula

let to_text_formula = TextFormula.of_formula text_formula_converter
let to_string = TextFormula.to_string % to_text_formula

let is = is % Entry.slug
let is' = Formula.pred % is

let tuneIs = tune % Tune.is'
let tuneIs' = Formula.pred % tuneIs

(* Little trick to convince OCaml that polymorphism is OK. *)
type op = {op: 'a. 'a Formula.t -> 'a Formula.t -> 'a Formula.t}

let optimise =
  let lift {op} f1 f2 =
    match (f1, f2) with
    | (Tune f1, Tune f2) -> Option.some @@ tune (op f1 f2)
    | (Kind f1, Kind f2) -> Option.some @@ kind (op f1 f2)
    | _ -> None
  in
  Formula.optimise
    ~lift_and: (lift {op = Formula.and_})
    ~lift_or: (lift {op = Formula.or_})
    (function
      | (Is _ as p) | (Key _ as p) -> p
      | Tune tfilter -> tune @@ Tune.optimise tfilter
      | Kind kfilter -> kind @@ Kind.Version.Filter.optimise kfilter
    )
