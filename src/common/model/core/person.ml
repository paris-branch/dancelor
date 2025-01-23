open Nes
open Dancelor_common_database

let _key = "person"

type t = {
  name: string;
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let name = name % Entry.value
let scddb_id = scddb_id % Entry.value
