open Nes

module Part_name = Part_name
module Structure = Structure
module Voices = Voices
module Content = Content

type source_core = {
  source: Source.t Entry.Id.t;
  structure: Structure.t;
  details: string; [@default ""]
}
[@@deriving eq, biniou, yojson, show {with_path = false}]

let source_core_have_same_source s1 s2 =
  Entry.Id.equal' s1.source s2.source

type source = {
  source: Source.t Entry.t;
  structure: Structure.t;
  details: string;
}

let source_to_source_core : source -> source_core = fun {source; structure; details} ->
  {source = Entry.id source; structure; details}

let source_source source = source.source
let source_structure source = source.structure
let source_details source = source.details

let _key = "version"

type t = {
  tune: Tune.t Entry.Id.t;
  key: Music.Key.t;
  sources: source_core list; [@default []]
  arrangers: Person.t Entry.Id.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: Content.t;
  (** In the client, we don't include the content, and it has to be retrieved by
      calling a specific endpoint; in the meantime, we fill it with [None]. *)
}
[@@deriving eq, biniou, yojson, make, show {with_path = false}, fields]

let make ~tune ~key ?sources ?arrangers ?remark ?disambiguation ~content () =
  let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
  let tune = Entry.id tune in
  let sources = Option.map (List.map source_to_source_core) sources in
  let arrangers = Option.map (List.map Entry.id) arrangers in
  make ~tune ~key ?sources ?arrangers ?remark ?disambiguation ~content ()

let tune' = tune % Entry.value
let key' = key % Entry.value
let remark' = remark % Entry.value
let disambiguation' = disambiguation % Entry.value
let content' = content % Entry.value

let set_content content version =
  {version with content}

let erase_lilypond_from_content version =
  {version with content = Content.erase_lilypond version.content}

let sources_grouped v =
  List.group ~by: source_core_have_same_source (sources v)
