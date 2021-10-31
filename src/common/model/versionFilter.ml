open Nes

let _key = "version-filter"

type predicate =
  | Is of VersionCore.t
  | Tune of TuneFilter.t
  | Key of Music.key
  | BarsEq of int | BarsNe of int
  | BarsGt of int | BarsGe of int
  | BarsLt of int | BarsLe of int
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is version = Formula.pred (Is version)
let tune tfilter = Formula.pred (Tune tfilter)
let tuneIs tune_ = tune (TuneFilter.is tune_)
let key key_ = Formula.pred (Key key_)
let barsEq bars_ = Formula.pred (BarsEq bars_)
let barsNe bars_ = Formula.pred (BarsNe bars_)
let barsGt bars_ = Formula.pred (BarsGt bars_)
let barsGe bars_ = Formula.pred (BarsGe bars_)
let barsLt bars_ = Formula.pred (BarsLt bars_)
let barsLe bars_ = Formula.pred (BarsLe bars_)

let raw string = tune (TuneFilter.raw string)

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "tune",    (tune @@@ TuneFilter.from_text_formula);
    "key",     raw_only ~convert:Music.key_of_string key;
    "bars",    raw_only ~convert:int_of_string barsEq;
    "bars-eq", raw_only ~convert:int_of_string barsEq;
    "bars-ne", raw_only ~convert:int_of_string barsNe;
    "bars-gt", raw_only ~convert:int_of_string barsGt;
    "bars-ge", raw_only ~convert:int_of_string barsGe;
    "bars-lt", raw_only ~convert:int_of_string barsLt;
    "bars-le", raw_only ~convert:int_of_string barsLe
  ]
  @ (List.map
       (fun (name, pred) ->
          (name, fun x -> tune (pred x)))
       TuneFilter.unary_text_predicates)

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
