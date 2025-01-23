open Nes
open Dancelor_common_model_utils
module Core = Dancelor_common_model_core

(* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
   of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
module DanceCore = struct include Core.Dance let equal _ _ = assert false end

type predicate =
  | Is of DanceCore.t Slug.t
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
