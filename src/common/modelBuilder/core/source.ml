open Nes

let _key = "source"

type t = {
  name: string;
  short_name: string; [@default ""] [@key "short-name"]
  editors: Person.t Slug.t list; [@default []]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  description: string option; [@default None]
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?short_name ?editors ?scddb_id ?description () =
  let name = String.remove_duplicates ~char: ' ' name in
  let editors = Option.map (List.map Entry.slug) editors in
  make ~name ?short_name ?editors ~scddb_id ~description ()

let name' = name % Entry.value
let short_name' = short_name % Entry.value
let scddb_id' = scddb_id % Entry.value
let description' = description % Entry.value
