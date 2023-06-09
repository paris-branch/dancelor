open Nes

let _key = "dance-filter"

type predicate =
  | Is of DanceCore.t
  | Name of string
  | NameMatches of string
  | Kind of KindFilter.Dance.t
  | Deviser of CreditFilter.t (** deviser is defined and passes the filter *)
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is dance = Formula.pred (Is dance)
let name name = Formula.pred (Name name)
let nameMatches name = Formula.pred (NameMatches name)
let kind kfilter = Formula.pred (Kind kfilter)
let deviser cfilter = Formula.pred (Deviser cfilter)

let raw string = Ok (nameMatches string)

let nullary_text_predicates = [
  "reel",       (kind KindFilter.(Dance.base (Base.is Reel)));       (* alias for kind:reel       FIXNE: make this clearer *)
  "jig",        (kind KindFilter.(Dance.base (Base.is Jig)));        (* alias for kind:jig        FIXNE: make this clearer *)
  "strathspey", (kind KindFilter.(Dance.base (Base.is Strathspey))); (* alias for kind:strathspey FIXNE: make this clearer *)
  "waltz",      (kind KindFilter.(Dance.base (Base.is Waltz)));      (* alias for kind:waltz      FIXNE: make this clearer *)
]

let unary_text_predicates =
  TextFormula.[
    "name",         raw_only ~convert:no_convert name;
    "name-matches", raw_only ~convert:no_convert nameMatches;
    "kind",         (kind @@@@ KindFilter.Dance.from_text_formula);
    "deviser",      (deviser @@@@ CreditFilter.from_text_formula);
    "by",           (deviser @@@@ CreditFilter.from_text_formula); (* alias for deviser; FIXME: make this clearer *)
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
