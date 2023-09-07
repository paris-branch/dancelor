open Nes

module PageCore = struct
  let _key = "book-page"

  type t =
    | Version of VersionCore.t Slug.t * VersionParameters.t
    | Set of SetCore.t Slug.t * SetParameters.t
    | InlineSet of SetCore.t * SetParameters.t
  [@@deriving yojson]
end

let _key = "book"

type t = {
  slug: t Slug.t;
  status: Status.t; [@default Status.bot]
  title: string;
  subtitle: string; [@default ""]
  short_title: string; [@default ""] [@key "short-title"]
  date: PartialDate.t option; [@default None]
  contents: PageCore.t list;
  source: bool; [@default false]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  modified_at: Datetime.t; [@key "modified-at"]
  created_at: Datetime.t [@key "created-at"]
}
[@@deriving make, yojson]

let slug book = Lwt.return book.slug
let status book = Lwt.return book.status
let contents book = Lwt.return book.contents

let contains_set set1 book =
  List.exists
    (
      function
      | PageCore.Set (set2, _) -> Slug.equal set1 set2
      | _ -> false
    )
    book.contents

type warning =
  | Empty
  | DuplicateSet of SetCore.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of TuneCore.t * (SetCore.t option * int) list
  (* DuplicateVersion contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | SetDanceMismatch of SetCore.t * DanceCore.t
  (* SetDanceMismatch contains a set where one of the associated dances
     does not have the same kind *)
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]

type page =
  | Version of VersionCore.t * VersionParameters.t
  | Set of SetCore.t * SetParameters.t
  | InlineSet of SetCore.t * SetParameters.t

module Filter = struct
  let _key = "book-filter"

  type predicate =
    | Is of t
    | IsSource
    | Title of string
    | TitleMatches of string
    | Subtitle of string
    | SubtitleMatches of string
    | ExistsVersion of VersionCore.Filter.t
    | ExistsSet of SetCore.Filter.t
    | ExistsInlineSet of SetCore.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]
end
