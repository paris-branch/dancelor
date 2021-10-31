open Nes

let _key = "set-filter"

type predicate =
  | Is of SetCore.t
  | Name of string
  | NameMatches of string
  | Deviser of CreditFilter.t (** deviser is defined and passes the filter *)
  | ExistsVersion of VersionFilter.t
  | Kind of KindFilter.Dance.t
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is set = Formula.pred (Is set)
let name name = Formula.pred (Name name)
let nameMatches name = Formula.pred (NameMatches name)
let deviser pfilter = Formula.pred (Deviser pfilter)
let existsVersion vfilter = Formula.pred (ExistsVersion vfilter)
let memVersion version = existsVersion (VersionFilter.is version)
let kind kfilter = Formula.pred (Kind kfilter)

let raw = nameMatches

let nullary_text_predicates = []

let unary_text_predicates =
  TextFormula.[
    "name",           raw_only ~convert:Fun.id name;
    "name-matches",   raw_only ~convert:Fun.id nameMatches;
    "deviser",        (deviser @@@ CreditFilter.from_text_formula);
    "exists-version", (existsVersion @@@ VersionFilter.from_text_formula);
    "kind",           (kind @@@ KindFilter.Dance.from_text_formula);
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
