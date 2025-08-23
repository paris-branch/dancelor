open Nes

let _key = "person"

type t = {
  name: NEString.t;
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?scddb_id () =
  let name = NEString.map_exn (String.remove_duplicates ~char: ' ') name in
  make ~name ~scddb_id ()

let name' = name % Entry.value
let scddb_id' = scddb_id % Entry.value
