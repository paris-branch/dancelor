open Nes

module Content = struct
  type part_name = char
  [@@deriving eq, yojson, show {with_path = false}]
  (* FIXME: limit to capitals *)

  type part = {
    melody: string;
    chords: string;
  }
  [@@deriving eq, yojson, show {with_path = false}]

  let lilypond_from_parts parts =
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
    | Full of string
    | Parts of (part_name * part) list
  [@@deriving eq, yojson, show {with_path = false}, variants]

  let lilypond = function
    | Full string -> string
    | Parts parts -> lilypond_from_parts parts
end

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
  content: Content.t option;
  (** In the client, we don't include the content, and it has to be retrieved by
      calling a specific endpoint; in the meantime, we fill it with [None]. *)
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

let content v =
  match v.content with
  | Some content -> content
  | None -> failwith "no content available"

let content' = content % Entry.value

let set_content content version =
  {version with content}
