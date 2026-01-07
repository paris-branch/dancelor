open Nes

module Page = struct
  type dance =
    | Dance_only
    | Dance_versions of (Version.t Entry.id * Version_parameters.t) NEList.t
    | Dance_set of Set.t Entry.id * Set_parameters.t
  [@@deriving eq, show {with_path = false}, yojson]

  type t =
    | Part of NEString.t
    | Dance of Dance.t Entry.id * dance
    | Versions of (Version.t Entry.Id.t * Version_parameters.t) NEList.t
    | Set of Set.t Entry.Id.t * Set_parameters.t
  [@@deriving eq, show {with_path = false}, yojson]
end

type page_dance =
  | Dance_only
  | Dance_versions of (Version.entry * Version_parameters.t) NEList.t
  | Dance_set of Set.entry * Set_parameters.t
[@@deriving show {with_path = false}, variants]

type page =
  | Part of NEString.t
  | Dance of Dance.entry * page_dance
  | Versions of (Version.entry * Version_parameters.t) NEList.t
  | Set of Set.entry * Set_parameters.t
[@@deriving show {with_path = false}, variants]

let page_dance_to_page_dance_core : page_dance -> Page.dance = function
  | Dance_only -> Page.Dance_only
  | Dance_versions versions_and_params -> Page.Dance_versions (NEList.map (Pair.map_fst Entry.id) versions_and_params)
  | Dance_set (set, params) -> Page.Dance_set (Entry.id set, params)

let page_to_page_core : page -> Page.t = function
  | Part title -> Page.Part title
  | Dance (dance, page_dance) -> Page.Dance (Entry.id dance, page_dance_to_page_dance_core page_dance)
  | Versions versions_and_params -> Page.Versions (NEList.map (Pair.map_fst Entry.id) versions_and_params)
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

type access = Entry.Access.Private.t [@@deriving yojson]
type entry = t Entry.private_
[@@deriving eq, show, yojson]

let make ~title ?authors ?date ?contents ?remark ?sources ?scddb_id () =
  let title = NEString.map_exn (String.remove_duplicates ~char: ' ') title in
  let authors = Option.map (List.map Entry.id) authors in
  let contents = Option.map (List.map page_to_page_core) contents in
  let sources = Option.map (List.map Entry.id) sources in
  make ~title ?authors ~date ?contents ?remark ?sources ~scddb_id ()

let title' = title % Entry.value_private_
let date' = date % Entry.value_private_
let remark' = remark % Entry.value_private_
let sources' = sources % Entry.value_private_
let scddb_id' = scddb_id % Entry.value_private_

let slug = NesSlug.of_string % NEString.to_string % title
let slug' = slug % Entry.value_private_

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
  | Duplicate_set of Set.entry (* FIXME: duplicate dance? *)
  | Duplicate_tune of Tune.entry * (Set.entry option * int) list
  (* Duplicate_tune contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | Set_dance_kind_mismatch of Set.entry * Dance.entry
(* Set_dance_kind_mismatch contains a set where one of the associated dances
   does not have the same kind *)
[@@deriving show {with_path = false}, yojson]

type warnings = warning list
[@@deriving show {with_path = false}, yojson]
