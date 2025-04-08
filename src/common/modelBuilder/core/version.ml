open Nes

let _key = "version"

type t = {
  tune: Tune.t Slug.t;
  bars: int;
  key: Music.key;
  structure: string;
  sources: Source.t Slug.t list; [@default []]
  arrangers: Person.t Slug.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: string;
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let tune = tune % Entry.value
let bars = bars % Entry.value
let key = key % Entry.value
let structure = structure % Entry.value
let sources = sources % Entry.value
let arrangers = arrangers % Entry.value
let remark = remark % Entry.value
let disambiguation = disambiguation % Entry.value
let content = content % Entry.value
