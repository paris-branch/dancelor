open Nes
open Dancelor_common_model_utils

type predicate =
  | Is of Dancelor_common_model_core.Book.t Slug.t
  | IsSource
  | Title of string
  | TitleMatches of string
  | Subtitle of string
  | SubtitleMatches of string
  | ExistsVersion of Version.t
  | ExistsSet of Set.t
  | ExistsInlineSet of Set.t
  | ExistsVersionDeep of Version.t
[@@deriving eq, show {with_path = false}, yojson, variants]

type t = predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]

let title' = Formula.pred % title
let titleMatches' = Formula.pred % titleMatches
let subtitle' = Formula.pred % subtitle
let subtitleMatches' = Formula.pred % subtitleMatches
let isSource' = Formula.pred IsSource
let existsVersion' = Formula.pred % existsVersion
let existsSet' = Formula.pred % existsSet
let existsInlineSet' = Formula.pred % existsInlineSet
let existsVersionDeep' = Formula.pred % existsVersionDeep
