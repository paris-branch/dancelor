open Nes

module Page = struct
  type t =
    | Version of Version.t Entry.Id.t * VersionParameters.t
    | Set of Set.t Entry.Id.t * SetParameters.t
    | InlineSet of Set.t * SetParameters.t
  [@@deriving eq, show {with_path = false}, yojson]
end

type page =
  | Version of Version.t Entry.t * VersionParameters.t
  | Set of Set.t Entry.t * SetParameters.t
  | InlineSet of Set.t * SetParameters.t
[@@deriving show {with_path = false}]

let page_to_page_core = function
  | (Version (version, params): page) -> Page.Version (Entry.id version, params)
  | (Set (set, params): page) -> Page.Set (Entry.id set, params)
  | (InlineSet (set, params): page) -> Page.InlineSet (set, params)

let _key = "book"

type t = {
  title: string;
  subtitle: string; [@default ""]
  short_title: string; [@default ""] [@key "short-title"]
  authors: Person.t Entry.Id.t list; [@default []]
  date: PartialDate.t option; [@default None]
  contents: Page.t list;
  source: bool; [@default false]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, make, show {with_path = false}, yojson, fields]

let make ~title ?subtitle ?short_title ?authors ?date ?(contents = []) ?source ?remark ?scddb_id () =
  let title = String.remove_duplicates ~char: ' ' title in
  let subtitle = Option.map (String.remove_duplicates ~char: ' ') subtitle in
  let short_title = Option.map (String.remove_duplicates ~char: ' ') short_title in
  let authors = Option.map (List.map Entry.id) authors in
  let contents = List.map page_to_page_core contents in
  make ~title ?subtitle ?short_title ?authors ~date ~contents ?source ?remark ~scddb_id ()

let title' = title % Entry.value
let subtitle' = subtitle % Entry.value
let short_title' = short_title % Entry.value
let date' = date % Entry.value
let source' = source % Entry.value
let remark' = remark % Entry.value
let scddb_id' = scddb_id % Entry.value

let slug = Entry.Slug.of_string % title
let slug' = slug % Entry.value

let contains_set set1 book =
  List.exists
    (function
      | Page.Set (set2, _) -> Entry.Id.equal' set1 set2
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
