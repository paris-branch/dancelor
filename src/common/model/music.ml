type note = A | B | C | D | E | F | G

let note_to_char = function
  | A -> 'a'
  | B -> 'b'
  | C -> 'c'
  | D -> 'd'
  | E -> 'e'
  | F -> 'f'
  | G -> 'g'

let note_of_char c =
  match Char.lowercase_ascii c with
  | 'a' -> A
  | 'b' -> B
  | 'c' -> C
  | 'd' -> D
  | 'e' -> E
  | 'f' -> F
  | 'g' -> G
  | _ -> failwith "Dancelor_common_model.Music.note_of_char"

let note_to_pretty_char n =
  note_to_char n
  |> Char.uppercase_ascii

type alteration = Flat | Sharp | Natural

let alteration_to_string = function
  | Flat -> "es"
  | Sharp -> "is"
  | Natural -> ""

let alteration_of_string = function
  | "es" -> Flat
  | "is" -> Sharp
  | "" -> Natural
  | _ -> failwith "Dancelor_common_model.Music.alteration_of_string"

let alteration_to_pretty_string = function
  | Flat -> "♭"
  | Sharp -> "♯"
  | Natural -> ""

type pitch = note * alteration

let pitch_to_pretty_string (n, a) =
  String.make 1 (note_to_pretty_char n)
  ^ alteration_to_pretty_string a

let pitch_to_string (n, a) =
  String.make 1 (note_to_char n)
  ^ alteration_to_string a

let pitch_of_string s =
  if String.length s > 0 then
    (
      note_of_char s.[0],
      alteration_of_string (String.sub s 1 (String.length s - 1))
    )
  else
    failwith "Dancelor_common_model.Music.pitch_of_string"

type mode = Major | Minor

let mode_to_string = function
  | Major -> ""
  | Minor -> ":m"

let mode_of_string = function
  | "" -> Major
  | ":m" -> Minor
  | _ -> failwith "Dancelor_common_model.Music.mode_of_string"

let mode_to_safe_string = function
  | Major -> ""
  | Minor -> "m"

let mode_to_pretty_string = function
  | Major -> ""
  | Minor -> "m"

type key = pitch * mode

let key_to_string (pitch, mode) =
  pitch_to_string pitch ^ mode_to_string mode

let key_of_string str =
  match String.index_opt str ':' with
  | None -> (pitch_of_string str, Major)
  | Some i -> (pitch_of_string (String.sub str 0 i),
               mode_of_string (String.sub str i (String.length str - i)))

let key_to_safe_string (pitch, mode) =
  pitch_to_string pitch
  ^ mode_to_safe_string mode

let key_to_pretty_string (pitch, mode) =
  pitch_to_pretty_string pitch
  ^ mode_to_pretty_string mode

let%test _ = let k = ((C, Flat), Minor) in
             key_of_string (key_to_string k) = k
let%test _ = let k = ((G, Natural), Major) in
             key_of_string (key_to_string k) = k
let%test _ = let k = ((F, Sharp), Major) in
             key_of_string (key_to_string k) = k

let key_to_yojson k =
  `String (key_to_string k)

let key_of_yojson = function
  | `String s ->
    (try Ok (key_of_string s)
     with _ -> Error "Dancelor_common_model.Music.key_of_yojson")
  | _ -> Error "Dancelor_common_model.Music.key_of_yojson"

type clef = Treble | Bass

let clef_to_string = function
  | Treble -> "treble"
  | Bass -> "bass"

let clef_to_symbol = function
  | Treble -> "𝄞"
  | Bass -> "𝄢"

let clef_of_string = function
  | "treble" -> Treble
  | "bass" -> Bass
  | _ -> failwith "clef_of_string"

let clef_to_yojson clef =
  `String (clef_to_string clef)

let clef_of_yojson = function
  | `String s ->
    (try Ok (clef_of_string s)
     with _ -> Error "Dancelor_common_model.Music.clef_of_yojson")
  | _ -> Error "Dancelor_common_model.Music.clef_of_yojson"
