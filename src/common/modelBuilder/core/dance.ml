open Nes

let _key = "dance"

type t = {
  names_: NEString.t NEList.t; [@key "names"] (* work around a name clash in ppx_fields_conv *)
  kind: Kind.Dance.t;
  devisers: Person.t Entry.Id.t list; [@default []]
  two_chords: bool option; [@default None] [@key "two-chords"]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  disambiguation: string; [@default ""]
  date: PartialDate.t option; [@default None] (** When the dance was devised. *)
}
[@@deriving eq, make, show {with_path = false}, yojson, fields]

type access = Entry.Access.public [@@deriving yojson]
type entry = t Entry.public
[@@deriving eq, show, yojson]

let make ~names ~kind ?devisers ?two_chords ?scddb_id ?disambiguation ?date () =
  let names = NEList.map (NEString.map_exn (String.remove_duplicates ~char: ' ')) names in
  let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
  let devisers = Option.map (List.map Entry.id) devisers in
  make ~names_: names ~kind ?devisers ~two_chords ~scddb_id ?disambiguation ~date ()

let names = names_
let names' = names % Entry.value_public
let one_name = NEList.hd % names
let one_name' = one_name % Entry.value_public
let other_names = NEList.tl % names
let other_names' = other_names % Entry.value_public

let kind' = kind % Entry.value_public
let two_chords' = two_chords % Entry.value_public
let scddb_id' = scddb_id % Entry.value_public
let disambiguation' = disambiguation % Entry.value_public
let date' = date % Entry.value_public

let slug = NesSlug.of_string % NEString.to_string % one_name
let slug' = slug % Entry.value_public
