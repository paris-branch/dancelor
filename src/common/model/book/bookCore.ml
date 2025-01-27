open Nes
open Dancelor_common_database

module PageCore = struct
  type t =
    | Version of VersionCore.t Slug.t * VersionParameters.t
    | Set of SetCore.t Slug.t * SetParameters.t
    | InlineSet of SetCore.t * SetParameters.t
  [@@deriving show {with_path = false}, yojson]
end

let _key = "book"

type t = {
  title: string;
  subtitle: string; [@default ""]
  short_title: string; [@default ""] [@key "short-title"]
  date: PartialDate.t option; [@default None]
  contents: PageCore.t list;
  source: bool; [@default false]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving make, show {with_path = false}, yojson, fields]

let make
    ~title
    ?subtitle
    ?short_title
    ?date
    ?(contents = [])
    ?source
    ?remark
    ?scddb_id
    ()
  =
  make
    ~title
    ?subtitle
    ?short_title
    ~date
    ~contents
    ?source
    ?remark
    ~scddb_id
    ()

let title = title % Entry.value
let subtitle = subtitle % Entry.value
let short_title = short_title % Entry.value
let date = date % Entry.value
let contents = contents % Entry.value
let source = source % Entry.value
let remark = remark % Entry.value
let scddb_id = scddb_id % Entry.value

let contains_set set1 book =
  List.exists
    (function
      | PageCore.Set (set2, _) -> Slug.equal' set1 set2
      | _ -> false
    )
    (Entry.value book).contents

type warning =
  | Empty
  | DuplicateSet of SetCore.t Entry.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of TuneCore.t Entry.t * (SetCore.t Entry.t option * int) list
  (* DuplicateVersion contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | SetDanceMismatch of SetCore.t Entry.t * DanceCore.t Entry.t
  (* SetDanceMismatch contains a set where one of the associated dances
     does not have the same kind *)
[@@deriving show {with_path = false}, yojson]

type warnings = warning list
[@@deriving show {with_path = false}, yojson]

type page =
  | Version of VersionCore.t Entry.t * VersionParameters.t
  | Set of SetCore.t Entry.t * SetParameters.t
  | InlineSet of SetCore.t * SetParameters.t
[@@deriving show {with_path = false}]

module Filter = struct
  (* Dirty trick to convince [ppx_deriving.std] that it can derive the equality
     of [t Slug.t]. [Slug.equal] ignores its first argument anyways. *)
  let equal _ _ = assert false

  type predicate =
    | Is of t Slug.t
    | IsSource
    | Title of string
    | TitleMatches of string
    | Subtitle of string
    | SubtitleMatches of string
    | ExistsVersion of VersionCore.Filter.t
    | ExistsSet of SetCore.Filter.t
    | ExistsInlineSet of SetCore.Filter.t
    | ExistsVersionDeep of VersionCore.Filter.t
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
end
