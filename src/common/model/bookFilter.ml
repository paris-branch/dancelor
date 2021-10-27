open Nes

let _key = "book-filter"

type predicate =
  | Is of BookCore.t
  | IsSource
  | Title of string
  | TitleMatches of string
  | Subtitle of string
  | SubtitleMatches of string
  | ExistsVersion of VersionFilter.t
  | ExistsSet of SetFilter.t
  | ExistsInlineSet of SetFilter.t
[@@deriving yojson]

type t = predicate Formula.t
[@@deriving yojson]

let is book = Formula.pred (Is book)
let title string = Formula.pred (Title string)
let titleMatches string = Formula.pred (TitleMatches string)
let subtitle string = Formula.pred (Subtitle string)
let subtitleMatches string = Formula.pred (SubtitleMatches string)
let isSource = Formula.pred IsSource
let existsVersion vfilter = Formula.pred (ExistsVersion vfilter)
let memVersion version = existsVersion (VersionFilter.is version)
let existsSet sfilter = Formula.pred (ExistsSet sfilter)
let memSet set = existsSet (SetFilter.is set)
let existsInlineSet sfilter = Formula.pred (ExistsInlineSet sfilter)

let existsVersionDeep vfilter =
  Formula.or_l [
    existsVersion vfilter;
    existsSet (SetFilter.existsVersion vfilter);
    existsInlineSet (SetFilter.existsVersion vfilter);
  ]
(** Check whether the given version filter can be satisfied in the book at any
    depth. This is different from {!existsVersion} which checks only in the
    direct list of version. *)

let memVersionDeep version = existsVersionDeep (VersionFilter.is version)

let existsTuneDeep tfilter = existsVersionDeep (VersionFilter.tune tfilter)
(** Checks whether the given tune filter can be satisfied in the book at any
    depth. *)

let memTuneDeep tune = existsTuneDeep (TuneFilter.is tune)

let raw string =
  Formula.or_l [
    titleMatches string;
    subtitleMatches string;
  ]

let nullary_text_predicates = [
  "source", isSource
]

let unary_text_predicates =
  TextFormula.[
    "title",             raw_only ~convert:Fun.id title;
    "title-matches",     raw_only ~convert:Fun.id titleMatches;
    "subtitle",          raw_only ~convert:Fun.id subtitle;
    "subtitle-matches",  raw_only ~convert:Fun.id subtitleMatches;
    "exists-version",    (existsVersion @@@ VersionFilter.from_text_formula);
    "exists-set",        (existsSet @@@ SetFilter.from_text_formula);
    "exists-inline-set", (existsInlineSet @@@ SetFilter.from_text_formula);
    "exists-version-deep", (existsVersionDeep @@@ VersionFilter.from_text_formula);
    "exists-tune-deep",    (existsVersionDeep @@@ VersionFilter.from_text_formula);
  ]

let from_text_formula =
  TextFormula.make_to_formula raw
    nullary_text_predicates
    unary_text_predicates
