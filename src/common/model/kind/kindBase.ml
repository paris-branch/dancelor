open Nes

let _key = "kind-base"

type t =
  | Jig
  | Polka
  | Reel
  | Strathspey
  | Waltz

let to_char = function
  | Jig -> 'J'
  | Polka -> 'P'
  | Reel -> 'R'
  | Strathspey -> 'S'
  | Waltz -> 'W'

let of_char c =
  match Char.uppercase_ascii c with
  | 'J' -> Jig
  | 'P' -> Polka
  | 'R' -> Reel
  | 'S' -> Strathspey
  | 'W' -> Waltz
  | _ -> invalid_arg "Dancelor_common_model.Kind.base_of_char"

let to_string b =
  String.make 1 (to_char b)

let of_string s =
  try of_char s.[0]
  with Invalid_argument _ | Failure _ ->
    invalid_arg "Dancelor_common_model.Kind.base_of_string"

let of_string_opt s =
  try Some (of_string s)
  with Invalid_argument _ -> None

let to_yojson b =
  `String (to_string b)

let of_yojson = function
  | `String s ->
    (try Ok (of_string s)
     with _ -> Error "Dancelor_common_model.Kind.base_of_yojson: not a valid base kind")
  | _ -> Error "Dancelor_common_model.Kind.base_of_yojson: not a JSON string"

let to_pretty_string ?(capitalised=false) base =
  (
    match base with
    | Jig -> "jig"
    | Polka -> "polka"
    | Reel -> "reel"
    | Strathspey -> "strathspey"
    | Waltz -> "waltz"
  )
  |> if capitalised then String.capitalize_ascii else Fun.id

let tempo = function
  | Jig -> ("4.", 108)
  | Polka | Reel -> ("2", 108)
  | Strathspey -> ("2", 60)
  | Waltz -> ("2.", 60)
