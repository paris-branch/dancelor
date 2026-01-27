open Nes

type composer_core = {
  composer: Person.t Entry.Id.t;
  details: string; [@default ""]
}
[@@deriving eq, yojson, show {with_path = false}]

type composer = {
  composer: Person.entry;
  details: string;
}

let composer_to_composer_core : composer -> composer_core = fun {composer; details} ->
  {composer = Entry.id composer; details}

let composer_composer composer = composer.composer
let composer_details composer = composer.details

let _key = "tune"

type t = {
  names_: NEString.t NEList.t; [@key "names"] (* work around a name clash in ppx_fields_conv *)
  kind: Kind.Base.t;
  composers: composer_core list; [@default []]
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
  let composers = Option.map (List.map composer_to_composer_core) composers in
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
