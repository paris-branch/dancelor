open Nes

type t = {
  melody: string;
  chords: string;
}
[@@deriving eq, yojson, show {with_path = false}, fields]

let empty = {melody = ""; chords = ""}

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
