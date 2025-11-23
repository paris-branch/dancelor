open Nes

type structure = Part_name.t NEList.t
[@@deriving eq, show {with_path = false}]

let structure_to_string = NEString.of_string_exn % String.of_seq % Seq.map Part_name.to_char % List.to_seq % NEList.to_list
let structure_of_string = Option.join % Option.map NEList.of_list % Monadise.Option.monadise_1_1 List.map Part_name.of_char % List.of_seq % String.to_seq % NEString.to_string

let structure_to_yojson s = `String (NEString.to_string @@ structure_to_string s)
let structure_of_yojson = function
  | `String s -> Option.to_result ~none: "not a valid structure" (Option.bind (NEString.of_string s) structure_of_string)
  | _ -> Error "not a string"

type part = {
  melody: string;
  chords: string;
}
[@@deriving eq, yojson, show {with_path = false}, fields]

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
    (Music.Note.to_lilypond_string @@ Music.Pitch.note @@ Music.Key.pitch key) ^
    " " ^ (Music.Mode.to_lilypond_string key.mode)
  in
  let parts = NEList.to_list parts in
  let melody =
    String.concat " \\section\\break " (
      List.mapi
        (fun part_name part ->
          spf "\\mark\\markup\\box{%c} %s" (Part_name.to_char part_name) part.melody
        )
        parts
    ) ^
      "\\fine"
  in
  let chords = String.concat " " (List.map (fun part -> part.chords) parts) in
  spf
    "<< \\new Voice {\\clef treble \\time %s \\key %s {%s}}\\new ChordNames {\\chordmode {%s}}>>"
    time
    key
    melody
    chords

type destructured = {
  parts: part NEList.t;
  transitions: (Part_name.open_ * Part_name.open_ * part) list;
  default_structure: structure;
}
[@@deriving eq, yojson, show {with_path = false}]

type t =
  | Monolithic of {lilypond: string; bars: int; structure: structure}
  | Destructured of destructured
[@@deriving eq, yojson, show {with_path = false}, variants]

let lilypond kind key = function
  | Monolithic {lilypond; _} -> lilypond
  | Destructured {parts; _} -> lilypond_from_parts kind key parts

let erase_lilypond = function
  | Monolithic {bars; structure; _} -> Monolithic {bars; structure; lilypond = ""}
  | Destructured {default_structure; _} ->
    Destructured {
      default_structure;
      parts = NEList.singleton {melody = ""; chords = ""};
      transitions = [];
    }
