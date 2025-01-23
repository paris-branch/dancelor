open Nes
open Dancelor_common_database

module Page = struct
  type t =
    | Version of Version.t Slug.t * VersionParameters.t
    | Set of Set.t Slug.t * SetParameters.t
    | InlineSet of Set.t * SetParameters.t
  [@@deriving eq, show {with_path = false}, yojson]
end

let _key = "book"

type t = {
  title: string;
  subtitle: string; [@default ""]
  short_title: string; [@default ""] [@key "short-title"]
  date: PartialDate.t option; [@default None]
  contents: Page.t list;
  source: bool; [@default false]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, make, show {with_path = false}, yojson, fields]

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
      | Page.Set (set2, _) -> Slug.equal' set1 set2
      | _ -> false
    )
    (Entry.value book).contents

type warning =
  | Empty
  | DuplicateSet of Set.t Entry.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of Tune.t Entry.t * (Set.t Entry.t option * int) list
  (* DuplicateVersion contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | SetDanceMismatch of Set.t Entry.t * Dance.t Entry.t
  (* SetDanceMismatch contains a set where one of the associated dances
     does not have the same kind *)
[@@deriving show {with_path = false}, yojson]

type warnings = warning list
[@@deriving show {with_path = false}, yojson]

type page =
  | Version of Version.t Entry.t * VersionParameters.t
  | Set of Set.t Entry.t * SetParameters.t
  | InlineSet of Set.t * SetParameters.t
[@@deriving show {with_path = false}]
