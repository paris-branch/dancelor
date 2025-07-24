open Nes

let _key = "version"

type t = {
  tune: Tune.t Entry.Id.t;
  bars: int;
  key: Music.key;
  structure: string;
  sources: Source.t Entry.Id.t list; [@default []]
  arrangers: Person.t Entry.Id.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: string;
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~tune ~bars ~key ~structure ?sources ?arrangers ?remark ?disambiguation ~content () =
  let structure = String.remove_duplicates ~char: ' ' structure in
  let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
  let tune = Entry.id tune in
  let sources = Option.map (List.map Entry.id) sources in
  let arrangers = Option.map (List.map Entry.id) arrangers in
  make ~tune ~bars ~key ~structure ?sources ?arrangers ?remark ?disambiguation ~content ()

let tune' = tune % Entry.value
let bars' = bars % Entry.value
let key' = key % Entry.value
let structure' = structure % Entry.value
let remark' = remark % Entry.value
let disambiguation' = disambiguation % Entry.value
let content' = content % Entry.value
