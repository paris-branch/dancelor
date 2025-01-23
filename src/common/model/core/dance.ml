open Nes
open Dancelor_common_database
open Dancelor_common_model_utils

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
[@@deriving make, show {with_path = false}, yojson, fields]

let name = name % Entry.value
let kind = kind % Entry.value
let devisers = devisers % Entry.value
let two_chords = two_chords % Entry.value
let scddb_id = scddb_id % Entry.value
let disambiguation = disambiguation % Entry.value
let date = date % Entry.value
