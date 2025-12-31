open Nes

let _key = "tune"

type t = {
  names_: NEString.t NEList.t; [@key "names"] (* work around a name clash in ppx_fields_conv *)
  kind: Kind.Base.t;
  composers: Person.t Entry.Id.t list; [@default []]
  dances: Dance.t Entry.Id.t list; [@default []]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  date: PartialDate.t option; [@default None] (** When the tune was composed. *)
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

type access = Entry.Access.public [@@deriving yojson]
type entry = t Entry.public
[@@deriving eq, show, yojson]

let make ~names ~kind ?composers ?dances ?remark ?scddb_id ?date () =
  let names = NEList.map (NEString.map_exn (String.remove_duplicates ~char: ' ')) names in
  let composers = Option.map (List.map Entry.id) composers in
  let dances = Option.map (List.map Entry.id) dances in
  make ~names_: names ~kind ?composers ?dances ?remark ~scddb_id ~date ()

let names = names_
let names' = names % Entry.value_public
let one_name = NEList.hd % names
let one_name' = one_name % Entry.value_public
let other_names = NEList.tl % names
let other_names' = other_names % Entry.value_public

let kind' = kind % Entry.value_public
let remark' = remark % Entry.value_public
let scddb_id' = scddb_id % Entry.value_public
let date' = date % Entry.value_public

let slug = NesSlug.of_string % NEString.to_string % one_name
let slug' = slug % Entry.value_public
