open Nes

let _key = "source"

type t = {
  name: string;
  scddb_id: int option; [@default None] [@key "scddb-id"]
  description: string option; [@default None]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?scddb_id ?description () =
  let name = String.remove_duplicates ~char: ' ' name in
  make ~name ~scddb_id ~description ()

let name' = name % Entry.value
let scddb_id' = scddb_id % Entry.value
let description' = description % Entry.value
