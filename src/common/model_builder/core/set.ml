open Nes

let _key = "set"

type t = {
  name: NEString.t;
  conceptors: Person.t Entry.id list; [@default []]
  kind: Kind.Dance.t;
  contents: (Version.t Entry.id * Version_parameters.t) list; [@key "versions-and-parameters"] [@default []]
  order: Set_order.t;
  remark: string; [@default ""]
}
[@@deriving eq, ord, yojson, make, show {with_path = false}, fields]

type access = Entry.Access.Private.t [@@deriving yojson]
type entry = t Entry.private_
[@@deriving eq, ord, show, yojson]

let make ~name ~conceptors ~kind ~contents ~order ~remark () =
  let name = NEString.map_exn (String.remove_duplicates ~char: ' ') name in
  make ~name ~conceptors ~kind ~contents ~order ~remark ()

let name' = name % Entry.value_private_
let kind' = kind % Entry.value_private_
let order' = order % Entry.value_private_
let remark' = remark % Entry.value_private_
let conceptors' = conceptors % Entry.value_private_
let contents' = contents % Entry.value_private_

let slug = NesSlug.of_string % NEString.to_string % name
let slug' = slug % Entry.value_private_

let set_contents contents set = {set with contents}

let find_context index set =
  let versions = List.map fst @@ contents set in
  List.findi_context (fun i _ -> i = index) versions
let find_context' index = find_context index % Entry.value_private_

type warning =
  | Empty
  | Duplicate_tune of Tune.entry
[@@deriving yojson]

type warnings = warning list
[@@deriving yojson]
