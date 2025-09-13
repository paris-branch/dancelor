open Nes

module Content = struct
  type part_name = char
  [@@deriving eq, yojson, show {with_path = false}]
  (* FIXME: limit to capitals *)

  type structure = part_name NEList.t
  [@@deriving eq, show {with_path = false}]

  let structure_to_string = NEString.of_string_exn % String.of_seq % List.to_seq % NEList.to_list
  let structure_of_string = NEList.of_list % List.of_seq % String.to_seq % NEString.to_string

  let structure_to_yojson s = `String (NEString.to_string @@ structure_to_string s)
  let structure_of_yojson = function
    | `String s -> Option.to_result ~none: "not a valid structure" (Option.bind (NEString.of_string s) structure_of_string)
    | _ -> Error "not a string"

  type part = {
    melody: string;
    chords: string;
    bars: int;
  }
  [@@deriving eq, yojson, show {with_path = false}]

  let lilypond_from_parts kind key parts =
    let time =
      match kind with
      | Kind.Base.Reel -> "2/2"
      | Jig -> "6/8"
      | Strathspey -> "4/4"
      | Waltz -> "3/4"
      | Polka -> "2/2"
    in
    let key =
      (Music.note_to_lilypond_string key.Music.pitch.note) ^
      " " ^ (match key.mode with Major -> "\\major" | Minor -> "\\minor")
    in
    let parts = NEList.to_list parts in
    let melody =
      String.concat " \\bar \"||\"\\break " (
        List.map
          (fun (part_name, part) ->
            spf "\\mark\\markup\\box{%c} %s" part_name part.melody
          )
          parts
      ) ^
        "\\bar \"|.\""
    in
    let chords = String.concat " " (List.map (fun (_part_name, part) -> part.chords) parts) in
    spf
      "<< \\new Voice {\\clef treble \\time %s \\key %s {%s}}\\new ChordNames {\\chordmode {%s}}>>"
      time
      key
      melody
      chords

  type t =
    | Monolithic of {lilypond: string; bars: int; structure: structure}
    | Destructured of {parts: (part_name * part) NEList.t; common_structures: structure NEList.t}
  [@@deriving eq, yojson, show {with_path = false}, variants]

  let lilypond kind key = function
    | Monolithic {lilypond; _} -> lilypond
    | Destructured {parts; _} -> lilypond_from_parts kind key parts

  let erase_lilypond = function
    | Monolithic {bars; structure; _} -> Monolithic {bars; structure; lilypond = ""}
    | Destructured {common_structures; _} -> Destructured {common_structures; parts = NEList.singleton ('X', {melody = ""; chords = ""; bars = 0})}
end

let _key = "version"

type t = {
  tune: Tune.t Entry.Id.t;
  key: Music.key;
  sources: (Source.t Entry.Id.t * Content.structure) list; [@default []]
  arrangers: Person.t Entry.Id.t list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: Content.t;
  (** In the client, we don't include the content, and it has to be retrieved by
      calling a specific endpoint; in the meantime, we fill it with [None]. *)
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let make ~tune ~key ?sources ?arrangers ?remark ?disambiguation ~content () =
  let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
  let tune = Entry.id tune in
  let sources = Option.map (List.map (Pair.map_fst Entry.id)) sources in
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
