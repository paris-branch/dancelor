open Nes

module Part_name = Part_name
module Structure = Structure
module Voices = Voices
module Content = Content

type source = {
  source: Source.t Entry.Id.t;
  structure: Structure.t;
  details: NEString.t option; [@default None]
}
[@@deriving eq, ord, yojson, show {with_path = false}]

let source_have_same_source s1 s2 =
  Entry.Id.equal' s1.source s2.source

let source_source source = source.source
let source_structure source = source.structure
let source_details source = source.details

let _key = "version"

type t = {
  tune: Tune.t Entry.id;
  key: Music.Key.t;
  sources: source list; [@default []]
  arrangers: Person.t Entry.id list; [@default []]
  remark: NEString.t option; [@default None]
  disambiguation: NEString.t option; [@default None]
  content: Content.t;
  (** In the client, we don't include the content, and it has to be retrieved by
      calling a specific endpoint; in the meantime, we fill it with [None]. *)
}
[@@deriving eq, ord, yojson, make, show {with_path = false}, fields]

type access = Entry.Access.public [@@deriving yojson]
type entry = t Entry.public
[@@deriving eq, ord, show, yojson]

let make ~tune ~key ~sources ~arrangers ~remark ~disambiguation ~content () =
  let disambiguation = Option.map (NEString.map_exn (String.remove_duplicates ~char: ' ')) disambiguation in
  make ~tune ~key ~sources ~arrangers ~remark ~disambiguation ~content ()

let tune' = tune % Entry.value_public
let key' = key % Entry.value_public
let remark' = remark % Entry.value_public
let disambiguation' = disambiguation % Entry.value_public
let content' = content % Entry.value_public
let sources' = sources % Entry.value_public

let set_content content version =
  {version with content}

let erase_lilypond_from_content version =
  {version with content = Content.erase_lilypond version.content}

let sources_grouped v =
  List.group ~by: source_have_same_source (sources v)
let sources_grouped' = sources_grouped % Entry.value_public
