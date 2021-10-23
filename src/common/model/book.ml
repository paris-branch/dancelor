open Nes

let _key = "book"

type page_slug =
  | Version       of Version.t Slug.t * VersionParameters.t
  | Set           of     Set.t Slug.t * SetParameters.t
  | InlineSet     of     Set.t        * SetParameters.t
[@@deriving yojson]

type t =
  { slug        : t Slug.t ;
    status      : Status.t  [@default Status.bot] ;
    title       : string ;
    subtitle    : string    [@default ""] ;
    short_title : string    [@default ""] [@key "short-title"] ;
    date        : Date.t    [@default Date.none] ;
    contents    : page_slug list ;
    source      : bool      [@default false] ;
    remark      : string    [@default ""] }
[@@deriving yojson]

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let title p = Lwt.return p.title
let short_title p = if p.short_title = "" then title p else Lwt.return p.short_title
let subtitle p = Lwt.return p.subtitle
let date p = Lwt.return p.date
let contents p = Lwt.return p.contents
let source p = Lwt.return p.source
let remark p = Lwt.return p.remark

let equal book1 book2 =
  let%lwt slug1 = slug book1 in
  let%lwt slug2 = slug book2 in
  Lwt.return (Slug.equal slug1 slug2)

let is_source p = source p

let contains_set slug1 p =
  List.exists
    (function
      | Set (slug2, _) -> Slug.equal slug1 slug2
      | _ -> false)
    p.contents

let compare p1 p2 =
  (* Compare first by date *)
  let c = compare p1.date p2.date in
  if c = 0 then
    compare p1 p2
  else
    c

type warning =
  | Empty
  | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of Tune.t
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

type page =
  | Version       of Version.t * VersionParameters.t
  | Set           of     Set.t * SetParameters.t
  | InlineSet     of     Set.t * SetParameters.t

module Filter = struct
  let _key = "book-filter"

  type predicate =
    | Is of t
    | IsSource
    | Title of string
    | TitleMatches of string
    | Subtitle of string
    | SubtitleMatches of string
    | ExistsVersion of Version.Filter.t
    | ExistsSet of Set.Filter.t
    | ExistsInlineSet of Set.Filter.t
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
  let memVersion version = existsVersion (Version.Filter.is version)
  let existsSet sfilter = Formula.pred (ExistsSet sfilter)
  let memSet set = existsSet (Set.Filter.is set)
  let existsInlineSet sfilter = Formula.pred (ExistsInlineSet sfilter)

  let existsVersionDeep vfilter =
    Formula.or_l [
      existsVersion vfilter;
      existsSet (Set.Filter.existsVersion vfilter);
      existsInlineSet (Set.Filter.existsVersion vfilter);
    ]
  (** Check whether the given version filter can be satisfied in the book at any
      depth. This is different from {!existsVersion} which checks only in the
      direct list of version. *)

  let memVersionDeep version = existsVersionDeep (Version.Filter.is version)

  let existsTuneDeep tfilter = existsVersionDeep (Version.Filter.tune tfilter)
  (** Checks whether the given tune filter can be satisfied in the book at any
      depth. *)

  let memTuneDeep tune = existsTuneDeep (Tune.Filter.is tune)

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
      "exists-version",    (existsVersion @@@ Version.Filter.from_text_formula);
      "exists-set",        (existsSet @@@ Set.Filter.from_text_formula);
      "exists-inline-set", (existsInlineSet @@@ Set.Filter.from_text_formula);
    ]

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
