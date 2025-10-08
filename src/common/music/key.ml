open Nes

type t =
  {pitch: Pitch.t; mode: Mode.t}
[@@deriving eq, show {with_path = false}, qcheck2]

let make pitch mode = {pitch; mode}
let pitch key = key.pitch
let mode key = key.mode

let with_pitch f key = {key with pitch = f key.pitch}

let to_string key = Pitch.to_string key.pitch ^ Mode.to_string key.mode
let to_pretty_string key = Pitch.to_pretty_string key.pitch ^ Mode.to_string key.mode

let of_string = function
  | "" -> failwith "Common.Music.Key.of_string"
  | str ->
    (* FIXME: dirty; does not use mode_of_string *)
    let pitch_str, mode =
      if str.[String.length str - 1] = 'm' then
          (String.sub str 0 (String.length str - 1), Mode.Minor)
      else
          (str, Mode.Major)
    in
      {pitch = Pitch.of_string pitch_str; mode}

let of_string_opt s =
  try
    Some (of_string s)
  with
    | Failure _ -> None

let to_yojson = Utils.to_yojson__of__to_string to_string
let of_yojson = Utils.of_yojson__of__of_string of_string "Common.Music.Key.of_yojson"
