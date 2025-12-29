open Nes

let _key = "person"

type t = {
  name: NEString.t;
  user: User.t Entry.Id.t option; [@default None]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  composed_tunes_are_public: bool; [@default false]
  published_tunes_are_public: bool; [@default false]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?user ?scddb_id ?composed_tunes_are_public ?published_tunes_are_public () =
  let name = NEString.map_exn (String.remove_duplicates ~char: ' ') name in
  let user = Option.map Entry.id user in
  make ~name ~user ~scddb_id ?composed_tunes_are_public ?published_tunes_are_public ()

let name' = name % Entry.value
let scddb_id' = scddb_id % Entry.value
let composed_tunes_are_public' = composed_tunes_are_public % Entry.value
let published_tunes_are_public' = published_tunes_are_public % Entry.value
