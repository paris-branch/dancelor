open Nes

module Content = struct
  type part_name = char
  [@@deriving eq, yojson, show {with_path = false}]
  (* FIXME: limit to capitals *)

  type structure = part_name list
  [@@deriving eq, show {with_path = false}]

  let structure_to_string = String.of_seq % List.to_seq
  let structure_of_string = List.of_seq % String.to_seq

  let structure_to_yojson s = `String (structure_to_string s)
  let structure_of_yojson = function
    | `String s -> Ok (structure_of_string s)
    | _ -> Error "not a string"

  type part = {
    melody: string;
    chords: string;
    bars: int;
  }
  [@@deriving eq, yojson, show {with_path = false}]

  let lilypond_from_parts parts =
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
      "<< \\new Voice {\\clef treble \\time 4/4 \\key d \\major {%s}}\\new ChordNames {\\chordmode {%s}}>>"
      melody
      chords

  type t =
    | Full of {lilypond: string; bars: int; structure: structure}
    | Parts of {parts: (part_name * part) NEList.t; common_structures: structure NEList.t}
  [@@deriving eq, yojson, show {with_path = false}, variants]

  let lilypond = function
    | Full {lilypond; _} -> lilypond
    | Parts {parts; _} -> lilypond_from_parts parts

  let erase_lilypond = function
    | Full {bars; structure; _} -> Full {bars; structure; lilypond = ""}
    | Parts {common_structures; _} -> Parts {common_structures; parts = NEList.singleton ('X', {melody = ""; chords = ""; bars = 0})}
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
