type t =
  Treble | Bass
[@@deriving eq, show {with_path = false}]

let to_string = function Treble -> "treble" | Bass -> "bass"

let of_string = function
  | "treble" -> Treble
  | "bass" -> Bass
  | _ -> failwith "Common.Music.Clef.of_string"

let to_symbol = function Treble -> "𝄞" | Bass -> "𝄢"

let to_yojson = Utils.to_yojson__of__to_string to_string
let of_yojson = Utils.of_yojson__of__of_string of_string "Common.Music.Clef.of_yojson"
