open Nes

type t = int (* number of semitones *)
[@@deriving eq, show {with_path = false}, yojson]

let identity = 0
let compose = (+)

let from_semitones = id
let to_semitones = id

let from_pitches ~source ~target = Music.pitch_diff target source

let from_pitches' ~source ~target_relative =
  let target = Music.pitch_relative_to ~reference: source target_relative in
  from_pitches ~source ~target

let target_pitch ~source t = Music.pitch_add source t
