open Nes

let _key = "kind-dance"

type t = int * KindVersion.t list

let to_string (repeats, versions) =
  List.map KindVersion.to_string versions
  |> String.concat " + "
  |> (if repeats = 1 || List.length versions = 1 then id else spf "(%s)")
  |> (if repeats = 1 then id else spf "%d x %s" repeats)

let of_string s =
  let s = String.remove_char ' ' s in
  let (repeats, s) =
    try ssf s "%dx%n" (fun repeats n -> (repeats, snd (String.split n s)))
    with Scanf.Scan_failure _ -> (1, s)
  in
  let s =
    try ssf s "(%[^)])%!" id
    with Scanf.Scan_failure _ -> s
  in
  (repeats, List.map KindVersion.of_string (String.split_on_char '+' s))

let check s =
  match of_string s with
  | exception (Invalid_argument _) -> false
  | exception (Scanf.Scan_failure _) -> false
  | exception (End_of_file) -> false
  | _ -> true

let%test _ = to_string (3, [32, Strathspey]) = "3 x 32 S"
let%test _ = to_string (1, [128, Jig]) = "128 J"
let%test _ = to_string (2, [(32, Strathspey); (24, Reel)]) = "2 x (32 S + 24 R)"

let%test _ = of_string "3 x ( 32 Strathspey )" = (3, [32, Strathspey])
let%test _ = of_string "(32W + 64R)" = (1, [(32, Waltz); (64, Reel)])
let%test _ = of_string "3x40J" = (3, [40, Jig])
let%test _ = of_string "32R" = (1, [32, Reel])
let%test _ =
  try ignore (of_string "R"); false
  with Invalid_argument _ -> true

let of_string_opt s =
  try Some (of_string s)
  with Invalid_argument _ -> None

let to_yojson d =
  `String (to_string d)

let of_yojson = function
  | `String s ->
    (try Ok (of_string s)
     with _ -> Error "Dancelor_common_model.Kind.of_yojson: not a valid dance kind")
  | _ -> Error "Dancelor_common_model.Kind.of_yojson: not a JSON string"

let to_pretty_string (repeats, versions) =
  versions
  |> List.map KindVersion.to_pretty_string
  |> String.concat " + "
  |> (if repeats = 1 || List.length versions = 1 then Fun.id else spf "(%s)")
  |> (if repeats = 1 then Fun.id else spf "%d x %s" repeats)
