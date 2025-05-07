open Nes

let _key = "tune"

type t = {
  name: string;
  alternative_names: string list; [@key "alternative-names"] [@default []]
  kind: Kind.Base.t;
  composers: Person.t Slug.t list; [@default []]
  dances: Dance.t Slug.t list; [@default []]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  date: PartialDate.t option; [@default None] (** When the tune was composed. *)
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let name' = name % Entry.value
let alternative_names' = alternative_names % Entry.value
let kind' = kind % Entry.value
let remark' = remark % Entry.value
let scddb_id' = scddb_id % Entry.value
let date' = date % Entry.value

let compare e1 e2 = Slug.compare_slugs_or ~fallback: Stdlib.compare Entry.slug' e1 e2
