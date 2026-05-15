open Nes

type page_dance =
  | Dance_only
  | Dance_versions of (Version.t Entry.id * Version_parameters.t) NEList.t
  | Dance_set of Set.t Entry.id * Set_parameters.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

type page =
  | Part of NEString.t
  | Dance of Dance.t Entry.id * page_dance
  | Versions of (Version.t Entry.id * Version_parameters.t) NEList.t
  | Set of Set.t Entry.id * Set_parameters.t
[@@deriving eq, ord, show {with_path = false}, yojson, variants]

let _key = "book"

type t = {
  name: NEString.t;
  authors: Person.t Entry.Id.t list; [@default []]
  date: PartialDate.t option; [@default None]
  contents: page list;
  remark: string; [@default ""]
  sources: Source.t Entry.Id.t list; [@default []]
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, ord, make, show {with_path = false}, yojson, fields]

type access = Entry.Access.Private.t [@@deriving yojson]
type entry = t Entry.private_
[@@deriving eq, ord, show, yojson]

let make ~name ~authors ~date ~contents ~remark ~sources ~scddb_id () =
  let name = NEString.map_exn (String.remove_duplicates ~char: ' ') name in
  make ~name ~authors ~date ~contents ~remark ~sources ~scddb_id ()

let name' = name % Entry.value_private_
let date' = date % Entry.value_private_
let remark' = remark % Entry.value_private_
let scddb_id' = scddb_id % Entry.value_private_
let authors' = authors % Entry.value_private_
let sources' = sources % Entry.value_private_
let contents' = contents % Entry.value_private_

let slug = NesSlug.of_string % NEString.to_string % name
let slug' = slug % Entry.value_private_

let versions_from_contents book =
  List.concat_map
    (function
      | Versions versions_and_params -> NEList.(to_list % map fst) versions_and_params
      | _ -> []
    )
    (contents book)
let versions_from_contents' = versions_from_contents % Entry.value_private_

let contains_set set1 book =
  List.exists
    (function
      | Set (set2, _) -> Entry.Id.equal' set1 set2
      | _ -> false
    )
    (Entry.value book).contents

let set_contents contents book = {book with contents}

type warning =
  | Empty
  | Duplicate_set of Set.t Entry.id (* FIXME: duplicate dance? *)
  | Duplicate_tune of Tune.t Entry.id * (Set.t Entry.id option * int) list
  (* Duplicate_tune contains the list of sets in which the tune appears, as
     well as the number of times this set is present *)
  | Set_dance_kind_mismatch of Set.t Entry.id * Dance.t Entry.id
(* Set_dance_kind_mismatch contains a set where one of the associated dances
   does not have the same kind *)
[@@deriving show {with_path = false}, yojson]

type warnings = warning list
[@@deriving show {with_path = false}, yojson]
