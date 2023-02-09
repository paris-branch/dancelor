open Nes

let to_yojson__of__to_string to_string value =
  `String (to_string value)

let of_yojson__of__of_string of_string message = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        _ -> Error message
    )
  | _ -> Error message

(* Note *)

type note = A | B | C | D | E | F | G

let note_to_char = function
  A -> 'a'
  | B -> 'b'
  | C -> 'c'
  | D -> 'd'
  | E -> 'e'
  | F -> 'f'
  | G -> 'g'

let note_to_string note = note_to_char note |> Char.uppercase_ascii |> String.make 1
let note_to_pretty_string = note_to_string
let note_to_lilypond_string note = note_to_char note |> String.make 1
let note_to_safe_string = note_to_lilypond_string

let note_of_char c =
  match Char.lowercase_ascii c with
  | 'a' -> A
  | 'b' -> B
  | 'c' -> C
  | 'd' -> D
  | 'e' -> E
  | 'f' -> F
  | 'g' -> G
  | _ -> failwith "Dancelor_common_model.Music.note_of_char"

(* Alteration *)

type alteration = Flat | Sharp | Natural

let alteration_to_string = function Flat -> "b" | Sharp -> "#" | Natural -> ""
let alteration_to_pretty_string = function Flat -> "♭" | Sharp -> "♯" | Natural -> ""
let alteration_to_lilypond_string = function Flat -> "es" | Sharp -> "is" | Natural -> ""
let alteration_to_safe_string = alteration_to_lilypond_string

let alteration_of_string = function
  | "b" -> Flat
  | "#" -> Sharp
  | "" -> Natural
  | _ -> failwith "Dancelor_common_model.Music.alteration_of_string"

(* let alteration_of_lilypond_string = function
 *   | "es" -> Flat
 *   | "is" -> Sharp
 *   | "" -> Natural
 *   | _ -> failwith "Dancelor_common_model.Music.alteration_of_lilypond_string" *)

(* Octave *)

type octave = int

let octave_to_string octave =
  if octave < 0 then String.make (-octave) ','
  else String.make octave '\''

let octave_to_pretty_string = octave_to_string
let octave_to_lilypond_string = octave_to_string

let octave_of_string = function
  | "" -> 0
  | str ->
    let chr = str.[0] in
    if String.exists (( <> ) chr) str then
      failwith "Dancelor_common_model.Music.octave_of_string";
    match chr with
    | '\'' -> String.length str
    | ',' -> -(String.length str)
    | _ -> failwith "Dancelor_common_model.Music.octave_of_string"

(* Pitch *)

type pitch = {
  note: note;
  alteration: alteration;
  octave: octave;
}

let make_pitch note alteration octave =
  { note; alteration; octave }
let pitch_note pitch = pitch.note
let pitch_alteration pitch = pitch.alteration
let pitch_octave pitch = pitch.octave

let pitch_c = make_pitch C Natural 0

let pitch_to_string pitch =
  note_to_string pitch.note
  ^ alteration_to_string pitch.alteration
  ^ octave_to_string pitch.octave

let pitch_to_pretty_string pitch =
  note_to_pretty_string pitch.note
  ^ alteration_to_pretty_string pitch.alteration
  ^ octave_to_pretty_string pitch.octave

let pitch_to_lilypond_string pitch =
  note_to_lilypond_string pitch.note
  ^ alteration_to_lilypond_string pitch.alteration
  ^ octave_to_lilypond_string pitch.octave

let pitch_to_safe_string ?(strict_octave = true) pitch =
  if strict_octave && pitch.octave <> 0 then
    failwith "Dancelor_common_model.Music.pitch_to_safe_string";
  note_to_safe_string pitch.note
  ^ alteration_to_safe_string pitch.alteration

let pitch_of_string = function
  | "" -> failwith "Dancelor_common_model.Music.pitch_of_string"
  | s ->
    let note_alteration_str, octave_str =
      (* FIXME: Dirty as fuck *)
      match String.index_opt s ',' with
      | Some i ->
        (String.sub s 0 i, String.sub s i (String.length s - i))
      | None ->
        match String.index_opt s '\'' with
        | Some i ->
          (String.sub s 0 i, String.sub s i (String.length s - i))
        | None ->
          (s, "")
    in
    let note_char = note_alteration_str.[0] in
    let alteration_str = String.sub note_alteration_str 1 (String.length note_alteration_str - 1) in
    {
      note = note_of_char note_char;
      alteration = alteration_of_string alteration_str;
      octave = octave_of_string octave_str
    }

let pitch_to_yojson = to_yojson__of__to_string pitch_to_string
let pitch_of_yojson = of_yojson__of__of_string pitch_of_string "Dancelor_common_model.Music.pitch_of_yojson"

(* Mode *)

type mode = Major | Minor

let mode_to_string = function Major -> "" | Minor -> "m"
let mode_to_pretty_string = mode_to_string
let mode_to_lilypond_string = function Major -> "" | Minor -> ":m"
let mode_to_safe_string = mode_to_string

(* let mode_of_string = function
 *   | "" -> Major
 *   | "m" -> Minor
 *   | _ -> failwith "Dancelor_common_model.Music.mode_of_string" *)

(* let mode_of_lilypond_string = function
 *   | "" -> Major
 *   | ":m" -> Minor
 *   | _ -> failwith "Dancelor_common_model.Music.mode_of_lilypond_string" *)

(* Key *)

type key = { pitch: pitch; mode: mode; }

let make_key pitch mode = { pitch; mode }
let key_pitch key = key.pitch

let key_to_string key = pitch_to_string key.pitch ^ mode_to_string key.mode
let key_to_pretty_string key = pitch_to_pretty_string key.pitch ^ mode_to_pretty_string key.mode
let key_to_lilypond_string key = pitch_to_lilypond_string key.pitch ^ mode_to_lilypond_string key.mode
let key_to_safe_string key = pitch_to_safe_string key.pitch ^ mode_to_safe_string key.mode

let key_of_string = function
  | "" -> failwith "Dancelor_common_model.Music.key_of_string"
  | str ->
    (* FIXME: dirty; does not use mode_of_string *)
    let pitch_str, mode =
      if str.[String.length str - 1] = 'm' then
        (String.sub str 0 (String.length str - 1), Minor)
      else
        (str, Major)
    in
    { pitch = pitch_of_string pitch_str; mode }

let key_of_string_opt s =
  try
    Some (key_of_string s)
  with
    Failure _ -> None

let key_to_yojson = to_yojson__of__to_string key_to_string
let key_of_yojson = of_yojson__of__of_string key_of_string "Dancelor_common_model.Music.key_of_yojson"

module Key = struct
  let _key = "key"
  type t = key
  let to_yojson = key_to_yojson
  let of_yojson = key_of_yojson
end

(* Clef*)

type clef = Treble | Bass

let clef_to_string = function Treble -> "treble" | Bass -> "bass"
let clef_to_pretty_string = clef_to_string
let clef_to_lilypond_string = clef_to_string

let clef_of_string = function
  | "treble" -> Treble
  | "bass" -> Bass
  | _ -> failwith "clef_of_string"

let clef_to_symbol = function Treble -> "𝄞" | Bass -> "𝄢"

let clef_to_yojson = to_yojson__of__to_string clef_to_string
let clef_of_yojson = of_yojson__of__of_string clef_of_string "Dancelor_common_model.Music.clef_of_yojson"
