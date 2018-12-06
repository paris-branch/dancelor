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
  | _ -> failwith "Dancelor_model.Music.note_of_char"

type alteration = Flat | Sharp | Natural

let alteration_to_string = function
  | Flat -> "es"
  | Sharp -> "is"
  | Natural -> ""

let alteration_of_string = function
  | "es" -> Flat
  | "is" -> Sharp
  | "" -> Natural
  | _ -> failwith "Dancelor_model.Music.alteration_of_string"

type pitch = note * alteration

let pitch_to_string (n, a) =
  String.make 1 (note_to_char n) ^ alteration_to_string a

let pitch_of_string s =
  if String.length s > 0 then
    (
      note_of_char s.[0],
      alteration_of_string (String.sub s 1 (String.length s - 1))
    )
  else
    failwith "Dancelor_model.Music.pitch_of_string"

type mode = Major | Minor

let mode_to_string = function
  | Major -> ""
  | Minor -> ":m"

let mode_of_string = function
  | "" -> Major
  | ":m" -> Minor
  | _ -> failwith "Dancelor_model.Music.mode_of_string"

type key = pitch * mode

let key_to_string (pitch, mode) =
  pitch_to_string pitch ^ mode_to_string mode

let key_of_string str =
  match String.index_opt str ':' with
  | None -> (pitch_of_string str, Major)
  | Some i -> (pitch_of_string (String.sub str 0 i),
               mode_of_string (String.sub str i (String.length str - i)))

let%test _ = let k = ((C, Flat), Minor) in
             key_of_string (key_to_string k) = k
let%test _ = let k = ((G, Natural), Major) in
             key_of_string (key_to_string k) = k
let%test _ = let k = ((F, Sharp), Major) in
             key_of_string (key_to_string k) = k

let key_to_jsonm k = `String (key_to_string k)
let key_of_jsonm = function
  | `String s -> key_of_string s
  | _ -> failwith "Dancelor_model.Music.key_of_jsonm"

let key_to_yaml = key_to_jsonm
let key_of_yaml = key_of_jsonm
