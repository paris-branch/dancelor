open Dancelor_common

let spf = Printf.sprintf
let ssf = Scanf.sscanf

(* ============================= [ Basic Kind ] ============================= *)

type base =
  | Jig
  | Reel
  | Strathspey
  | Waltz

let base_to_char = function
  | Jig -> 'J'
  | Reel -> 'R'
  | Strathspey -> 'S'
  | Waltz -> 'W'

let base_to_string = function
  | Jig -> "jig"
  | Reel -> "reel"
  | Strathspey -> "strathspey"
  | Waltz -> "waltz"

let base_of_char c =
  match Char.uppercase_ascii c with
  | 'J' -> Jig
  | 'R' -> Reel
  | 'S' -> Strathspey
  | 'W' -> Waltz
  | _ -> failwith "Dancelor_model.Kind.base_of_char"

let base_of_string s =
  try base_of_char s.[0]
  with Failure _ -> failwith "Dancelor_model.Kind.base_of_string"

(* ============================= [ Tune Kind ] ============================== *)

type tune = int * base

let tune_to_string (repeats, base) =
    spf "%d%c" repeats (base_to_char base)

let tune_of_string s =
  let s = String.remove_char ' ' s in
  try
    ssf s "%d%[a-zA-Z]"
      (fun repeats base -> (repeats, base_of_string base))
  with
    Scanf.Scan_failure _ ->
    try
      ssf s "%[a-zA-Z]%d"
        (fun base repeats -> (repeats, base_of_string base))
    with
      Scanf.Scan_failure _ ->
      raise (Invalid_argument "Dancelor_model.Kind.tune_of_string")

let%test _ = tune_to_string (32, Waltz) = "32W"
let%test _ = tune_to_string (64, Reel) = "64R"
let%test _ = tune_to_string (24, Jig) = "24J"
let%test _ = tune_to_string (48, Strathspey) = "48S"

let%test _ = tune_of_string "W 32" = (32, Waltz)
let%test _ = tune_of_string "64 Reel" = (64, Reel)
let%test _ = tune_of_string "JIG 24" = (24, Jig)
let%test _ = tune_of_string "48 sTrathPEY" = (48, Strathspey)

(* ============================= [ Dance Kind ] ============================= *)

type dance =
  int * tune list

let dance_to_string (repeats, tunes) =
  List.map tune_to_string tunes
  |> String.concat " + "
  |> (if List.length tunes = 1 then id else spf "(%s)")
  |> (if repeats = 1 then id else spf "%dx%s" repeats)

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
  (repeats, List.map tune_of_string (String.split_on_char '+' s))

let%test _ = dance_to_string (3, [32, Strathspey]) = "3x32S"
let%test _ = dance_to_string (1, [128, Jig]) = "128J"
let%test _ = dance_to_string (2, [(32, Strathspey); (24, Reel)]) = "2x(32S + 24R)"

let%test _ = dance_of_string "3 x ( 32 Strathspey )" = (3, [32, Strathspey])
let%test _ = dance_of_string "(32W + 64R)" = (1, [(32, Waltz); (64, Reel)])
let%test _ = dance_of_string "3x40J" = (3, [40, Jig])
