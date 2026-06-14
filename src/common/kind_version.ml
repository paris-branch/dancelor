open Nes

type t = int * Kind_base.t
[@@deriving eq, ord, show {with_path = false}]

let to_string (repeats, base) =
  spf "%d %s" repeats (Kind_base.to_short_string base)

let of_string s =
  let (part1, part2) = Option.get @@ String.split_2_on_char ' ' s in
  try
    (int_of_string part1, Kind_base.of_string part2)
  with
    | _ -> (int_of_string part2, Kind_base.of_string part1)

let%test _ = to_string (32, Waltz) = "32 W"
let%test _ = to_string (64, Reel) = "64 R"
let%test _ = to_string (24, Jig) = "24 J"
let%test _ = to_string (48, Strathspey) = "48 S"

let%test _ = of_string "W 32" = (32, Waltz)
let%test _ = of_string "64 Reel" = (64, Reel)
let%test _ = of_string "JIG 24" = (24, Jig)
let%test _ = of_string "48 sTrathsPEY" = (48, Strathspey)

let%test _ =
  try
    ignore (of_string "R"); false
  with
    | Invalid_argument _ -> true
let%test _ =
  try
    ignore (of_string "8x32R"); false
  with
    | Invalid_argument _ -> true

let of_string_opt string =
  try
    Some (of_string string)
  with
    | Invalid_argument _ -> None

let to_yojson t =
  `String (to_string t)

let of_yojson = function
  | `String s ->
    (
      try
        Ok (of_string s)
      with
        | _ -> Error "Dancelor_common.Model.Kind.version_of_yojson: not a valid version kind"
    )
  | _ -> Error "Dancelor_common.Model.Kind.version_of_yojson: not a JSON string"

let to_pretty_string (repeats, base) =
  spf "%d %s" repeats (Kind_base.to_long_string ~capitalised: true base)
