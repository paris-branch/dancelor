open Nes

module Page = struct
  type dance =
    | DanceOnly
    | DanceVersion of Version.t Entry.Id.t * VersionParameters.t
    | DanceSet of Set.t Entry.Id.t * SetParameters.t
  [@@deriving eq, show {with_path = false}, yojson]

  type t =
    | Part of NEString.t
    | Dance of Dance.t Entry.Id.t * dance
    | Version of Version.t Entry.Id.t * VersionParameters.t
    | Set of Set.t Entry.Id.t * SetParameters.t
  [@@deriving eq, show {with_path = false}, yojson]
end

type page_dance =
  | DanceOnly
  | DanceVersion of Version.t Entry.t * VersionParameters.t
  | DanceSet of Set.t Entry.t * SetParameters.t
[@@deriving show {with_path = false}, variants]

type page =
  | Part of NEString.t
  | Dance of Dance.t Entry.t * page_dance
  | Version of Version.t Entry.t * VersionParameters.t
  | Set of Set.t Entry.t * SetParameters.t
[@@deriving show {with_path = false}, variants]

let page_dance_to_page_dance_core : page_dance -> Page.dance = function
  | DanceOnly -> Page.DanceOnly
  | DanceVersion (version, params) -> Page.DanceVersion (Entry.id version, params)
  | DanceSet (set, params) -> Page.DanceSet (Entry.id set, params)

let page_to_page_core : page -> Page.t = function
  | Part title -> Page.Part title
  | Dance (dance, page_dance) -> Page.Dance (Entry.id dance, page_dance_to_page_dance_core page_dance)
  | Version (version, params) -> Page.Version (Entry.id version, params)
  | Set (set, params) -> Page.Set (Entry.id set, params)

let _key = "book"

type t = {
  title: NEString.t;
  authors: Person.t Entry.Id.t list; [@default []]
  date: PartialDate.t option; [@default None]
  contents: Page.t list;
  remark: string; [@default ""]
  sources: Source.t Entry.Id.t list; [@default []]
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, make, show {with_path = false}, yojson, fields]

let make ~title ?authors ?date ?contents ?remark ?sources ?scddb_id () =
  let title = NEString.map_exn (String.remove_duplicates ~char: ' ') title in
  let authors = Option.map (List.map Entry.id) authors in
  let contents = Option.map (List.map page_to_page_core) contents in
  let sources = Option.map (List.map Entry.id) sources in
  make ~title ?authors ~date ?contents ?remark ?sources ~scddb_id ()

let title' = title % Entry.value
let date' = date % Entry.value
let remark' = remark % Entry.value
let sources' = sources % Entry.value
let scddb_id' = scddb_id % Entry.value

let slug = Entry.Slug.of_string % NEString.to_string % title
let slug' = slug % Entry.value

let contains_set set1 book =
  List.exists
    (function
      | Page.Set (set2, _) -> Entry.Id.equal' set1 set2
      | _ -> false
    )
    (Entry.value book).contents

let set_contents contents book =
  {book with contents = List.map page_to_page_core contents}

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
