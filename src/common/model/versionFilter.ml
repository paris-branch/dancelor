open Nes

let _key = "version-filter"

type predicate =
  | Is of VersionCore.t
  | Tune of TuneFilter.t
  | Key of Music.key
  | Kind of KindFilter.Version.t
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is version = Formula.pred (Is version)
let tune tfilter = Formula.pred (Tune tfilter)
let tuneIs tune_ = tune (TuneFilter.is tune_)
let key key_ = Formula.pred (Key key_)
let kind kfilter = Formula.pred (Kind kfilter)

let raw string = tune (TuneFilter.raw string)

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "tune",    (tune @@@ TuneFilter.from_text_formula);
    "key",     raw_only ~convert:Music.key_of_string key;
    "kind",    (kind @@@ KindFilter.Version.from_text_formula);
  ]
  @ (List.map
       (fun (name, pred) ->
          (name, fun x -> tune (pred x)))
       TuneFilter.unary_text_predicates)

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
