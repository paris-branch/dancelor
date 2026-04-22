type t =
  Treble | Alto | Tenor | Bass
[@@deriving eq, ord, show {with_path = false}]

let to_string = function
  | Treble -> "treble"
  | Alto -> "alto"
  | Tenor -> "tenor"
  | Bass -> "bass"

let of_string = function
  | "treble" -> Treble
  | "alto" -> Alto
  | "tenor" -> Tenor
  | "bass" -> Bass
  | _ -> failwith "Dancelor_common.Music.Clef.of_string"

let to_symbol = function
  | Treble -> "𝄞"
  | Alto -> "𝄡₃"
  | Tenor -> "𝄡₄"
  | Bass -> "𝄢"

let to_yojson = Utils.to_yojson__of__to_string to_string
let of_yojson = Utils.of_yojson__of__of_string of_string "Dancelor_common.Music.Clef.of_yojson"
