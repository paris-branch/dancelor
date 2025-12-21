open Nes

type t =
  A | B | C | D | E | F | G
[@@deriving eq, show {with_path = false}, qcheck2, biniou]

let to_char = function
  | A -> 'a'
  | B -> 'b'
  | C -> 'c'
  | D -> 'd'
  | E -> 'e'
  | F -> 'f'
  | G -> 'g'

let to_string note = to_char note |> Char.uppercase_ascii |> String.make 1
let to_lilypond_string note = to_char note |> String.make 1

let of_char c =
  match Char.lowercase_ascii c with
  | 'a' -> A
  | 'b' -> B
  | 'c' -> C
  | 'd' -> D
  | 'e' -> E
  | 'f' -> F
  | 'g' -> G
  | _ -> failwith "Common.Music.note_of_char"
