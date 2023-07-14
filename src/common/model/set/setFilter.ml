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
let deviser cfilter = Formula.pred (Deviser cfilter)
let existsVersion vfilter = Formula.pred (ExistsVersion vfilter)
let memVersion version = existsVersion (VersionFilter.is version)
let kind kfilter = Formula.pred (Kind kfilter)

let raw string = Ok (nameMatches string)

let nullary_text_predicates = [
  "reel",       (kind KindFilter.(Dance.base (Base.is Reel)));       (* alias for kind:reel       FIXNE: make this clearer *)
  "jig",        (kind KindFilter.(Dance.base (Base.is Jig)));        (* alias for kind:jig        FIXNE: make this clearer *)
  "strathspey", (kind KindFilter.(Dance.base (Base.is Strathspey))); (* alias for kind:strathspey FIXNE: make this clearer *)
  "waltz",      (kind KindFilter.(Dance.base (Base.is Waltz)));      (* alias for kind:waltz      FIXNE: make this clearer *)
]

let unary_text_predicates =
  TextFormula.[
    "name",           raw_only ~convert:no_convert name;
    "name-matches",   raw_only ~convert:no_convert nameMatches;
    "deviser",        (deviser @@@@ CreditFilter.from_text_formula);
    "by",             (deviser @@@@ CreditFilter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
    "exists-version", (existsVersion @@@@ VersionFilter.from_text_formula);
    "kind",           (kind @@@@ KindFilter.Dance.from_text_formula);
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates

let from_string ?filename input =
  from_text_formula (TextFormula.from_string ?filename input)
