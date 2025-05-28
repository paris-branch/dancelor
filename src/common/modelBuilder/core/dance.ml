open Nes

let _key = "dance"

type t = {
  name: string;
  kind: Kind.Dance.t;
  devisers: Person.t Slug.t list; [@default []]
  two_chords: bool option; [@default None] [@key "two-chords"]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  disambiguation: string; [@default ""]
  date: PartialDate.t option; [@default None] (** When the dance was devised. *)
}
[@@deriving eq, make, show {with_path = false}, yojson, fields]

let make ~name ~kind ?devisers ?two_chords ?scddb_id ?disambiguation ?date () =
  let name = String.remove_duplicates ~char: ' ' name in
  let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
  let devisers = Option.map (List.map Entry.slug) devisers in
  make ~name ~kind ?devisers ~two_chords ~scddb_id ?disambiguation ~date ()

let name' = name % Entry.value
let kind' = kind % Entry.value
let two_chords' = two_chords % Entry.value
let scddb_id' = scddb_id % Entry.value
let disambiguation' = disambiguation % Entry.value
let date' = date % Entry.value
