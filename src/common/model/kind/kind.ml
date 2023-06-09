open Nes

(* ============================= [ Basic Kind ] ============================= *)

type base =
  | Jig
  | Polka
  | Reel
  | Strathspey
  | Waltz

let base_to_char = function
  | Jig -> 'J'
  | Polka -> 'P'
  | Reel -> 'R'
  | Strathspey -> 'S'
  | Waltz -> 'W'

let base_of_char c =
  match Char.uppercase_ascii c with
  | 'J' -> Jig
  | 'P' -> Polka
  | 'R' -> Reel
  | 'S' -> Strathspey
  | 'W' -> Waltz
  | _ -> invalid_arg "Dancelor_common_model.Kind.base_of_char"

let base_to_string b =
  String.make 1 (base_to_char b)

let base_of_string s =
  try base_of_char s.[0]
  with Invalid_argument _ | Failure _ ->
    invalid_arg "Dancelor_common_model.Kind.base_of_string"

let base_of_string_opt s =
  try Some (base_of_string s)
  with Invalid_argument _ -> None

let base_to_yojson b =
  `String (base_to_string b)

let base_of_yojson = function
  | `String s ->
    (try Ok (base_of_string s)
     with _ -> Error "Dancelor_common_model.Kind.base_of_yojson: not a valid base kind")
  | _ -> Error "Dancelor_common_model.Kind.base_of_yojson: not a JSON string"

let base_to_pretty_string ?(capitalised=false) base =
  (
    match base with
    | Jig -> "jig"
    | Polka -> "polka"
    | Reel -> "reel"
    | Strathspey -> "strathspey"
    | Waltz -> "waltz"
  )
  |> if capitalised then String.capitalize_ascii else Fun.id

let base_tempo = function
  | Jig -> ("4.", 108)
  | Polka | Reel -> ("2", 108)
  | Strathspey -> ("2", 60)
  | Waltz -> ("2.", 60)

module Base = struct
  type t = base
  let _key = "kind-base"
  let to_yojson = base_to_yojson
  let of_yojson = base_of_yojson
end

(* ============================= [ Version Kind ] ============================== *)

type version = int * base

let version_to_string (repeats, base) =
  spf "%d %s" repeats (base_to_string base)

let version_of_string s =
  let s = NesString.remove_char ' ' s in
  try
    ssf s "%d%[a-zA-Z]"
      (fun repeats base -> (repeats, base_of_string base))
  with
  | End_of_file | Scanf.Scan_failure _ ->
    try
      ssf s "%[a-zA-Z]%d"
        (fun base repeats -> (repeats, base_of_string base))
    with
    | End_of_file | Scanf.Scan_failure _ ->
      invalid_arg "Dancelor_common_model.Kind.version_of_string"

let%test _ = version_to_string (32, Waltz) = "32 W"
let%test _ = version_to_string (64, Reel) = "64 R"
let%test _ = version_to_string (24, Jig) = "24 J"
let%test _ = version_to_string (48, Strathspey) = "48 S"

let%test _ = version_of_string "W 32" = (32, Waltz)
let%test _ = version_of_string "64 Reel" = (64, Reel)
let%test _ = version_of_string "JIG 24" = (24, Jig)
let%test _ = version_of_string "48 sTrathPEY" = (48, Strathspey)

let%test _ =
  try ignore (version_of_string "R"); false
  with Invalid_argument _ -> true
let%test _ =
  try ignore (version_of_string "8x32R"); false
  with Invalid_argument _ -> true

let version_of_string_opt string =
  try Some (version_of_string string)
  with Invalid_argument _ -> None

let version_to_yojson t =
  `String (version_to_string t)

let version_of_yojson = function
  | `String s ->
    (try Ok (version_of_string s)
     with _ -> Error "Dancelor_common_model.Kind.version_of_yojson: not a valid version kind")
  | _ -> Error "Dancelor_common_model.Kind.version_of_yojson: not a JSON string"

let version_to_pretty_string (repeats, base) =
  spf "%d %s" repeats (base_to_pretty_string ~capitalised:true base)

module Version = struct
  type t = version
  let _key = "kind-version"
  let to_yojson = version_to_yojson
  let of_yojson = version_of_yojson
end

(* ============================= [ Dance Kind ] ============================= *)

type dance =
  int * version list

let dance_to_string (repeats, versions) =
  List.map version_to_string versions
  |> String.concat " + "
  |> (if repeats = 1 || List.length versions = 1 then id else spf "(%s)")
  |> (if repeats = 1 then id else spf "%d x %s" repeats)

let dance_of_string s =
  let s = String.remove_char ' ' s in
  let (repeats, s) =
    try ssf s "%dx%n" (fun repeats n -> (repeats, snd (String.split n s)))
    with Scanf.Scan_failure _ -> (1, s)
  in
  let s =
    try ssf s "(%[^)])%!" id
    with Scanf.Scan_failure _ -> s
  in
  (repeats, List.map version_of_string (String.split_on_char '+' s))

let check_dance s =
  match dance_of_string s with
  | exception (Invalid_argument _) -> false
  | exception (Scanf.Scan_failure _) -> false
  | exception (End_of_file) -> false
  | _ -> true

let%test _ = dance_to_string (3, [32, Strathspey]) = "3 x 32 S"
let%test _ = dance_to_string (1, [128, Jig]) = "128 J"
let%test _ = dance_to_string (2, [(32, Strathspey); (24, Reel)]) = "2 x (32 S + 24 R)"

let%test _ = dance_of_string "3 x ( 32 Strathspey )" = (3, [32, Strathspey])
let%test _ = dance_of_string "(32W + 64R)" = (1, [(32, Waltz); (64, Reel)])
let%test _ = dance_of_string "3x40J" = (3, [40, Jig])
let%test _ = dance_of_string "32R" = (1, [32, Reel])
let%test _ =
  try ignore (dance_of_string "R"); false
  with Invalid_argument _ -> true

let dance_of_string_opt s =
  try Some (dance_of_string s)
  with Invalid_argument _ -> None

let dance_to_yojson d =
  `String (dance_to_string d)

let dance_of_yojson = function
  | `String s ->
    (try Ok (dance_of_string s)
     with _ -> Error "Dancelor_common_model.Kind.dance_of_yojson: not a valid dance kind")
  | _ -> Error "Dancelor_common_model.Kind.dance_of_yojson: not a JSON string"

let dance_to_pretty_string (repeats, versions) =
  versions
  |> List.map version_to_pretty_string
  |> String.concat " + "
  |> (if repeats = 1 || List.length versions = 1 then Fun.id else spf "(%s)")
  |> (if repeats = 1 then Fun.id else spf "%d x %s" repeats)

module Dance = struct
  type t = dance
  let _key = "kind-dance"
  let to_yojson = dance_to_yojson
  let of_yojson = dance_of_yojson
end
