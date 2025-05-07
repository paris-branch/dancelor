open Nes

let _key = "person"

type t = {
  name: string;
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?scddb_id () = make ~name ~scddb_id ()

let name' = name % Entry.value
let scddb_id' = scddb_id % Entry.value

let trad_slug : t Slug.t = Slug.check_string_exn "traditional"
let is_trad' c = Slug.equal' (Entry.slug c) trad_slug
