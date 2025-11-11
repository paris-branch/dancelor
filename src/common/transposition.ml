open Nes

type t = int (* number of semitones *)
[@@deriving eq, show {with_path = false}, yojson]

let identity = 0
let compose = (+)

let from_semitones = id
let to_semitones = id

let target_pitch ~source t = Music.Pitch.add source t
