open Nes

let _key = "source"

type t = {
  name: string;
  short_name: string; [@default ""] [@key "short-name"]
  editors: Person.t Entry.Id.t list; [@default []]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  description: string option; [@default None]
  date: PartialDate.t option; [@default None] (** When the source was published. *)
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~name ?short_name ?editors ?scddb_id ?description ?date () =
  let name = String.remove_duplicates ~char: ' ' name in
  let editors = Option.map (List.map Entry.id) editors in
  make ~name ?short_name ?editors ~scddb_id ~description ~date ()

let name' = name % Entry.value
let short_name' = short_name % Entry.value
let scddb_id' = scddb_id % Entry.value
let description' = description % Entry.value
let date' = date % Entry.value
