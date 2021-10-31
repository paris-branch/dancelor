open Nes

let _key = "tune-filter"

type predicate =
  | Is of TuneCore.t
  | Name of string
  | NameMatches of string
  | Author of CreditFilter.t (** author is defined and passes the filter *)
  | Kind of KindFilter.Base.t
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is tune = Formula.pred (Is tune)
let name string = Formula.pred (Name string)
let nameMatches string = Formula.pred (NameMatches string)
let author cfilter = Formula.pred (Author cfilter)
let authorIs author_ = author (CreditFilter.is author_)
let kind kfilter = Formula.pred (Kind kfilter)

let raw = nameMatches

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "name",         raw_only ~convert:Fun.id name;
    "name-matches", raw_only ~convert:Fun.id nameMatches;
    "author",       (author @@@ CreditFilter.from_text_formula);
    "kind",         (kind @@@ KindFilter.Base.from_text_formula);
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
