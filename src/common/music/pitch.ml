open Nes

type alteration =
  Flat | Sharp | Natural
[@@deriving eq, show {with_path = false}, qcheck2]

let alteration_to_string = function Flat -> "b" | Sharp -> "#" | Natural -> ""
let alteration_to_pretty_string = function Flat -> "♭" | Sharp -> "♯" | Natural -> ""
let alteration_to_lilypond_string = function Flat -> "es" | Sharp -> "is" | Natural -> ""

let alteration_of_string = function
  | "b" -> Flat
  | "#" -> Sharp
  | "" -> Natural
  | _ -> failwith "Common.Music.alteration_of_string"

type octave = int
[@@deriving eq, show {with_path = false}]

let gen_octave = QCheck2.Gen.int_range (-8) 8

let octave_to_lilypond_string octave =
  if octave < 0 then String.make (-octave) ','
  else String.make octave '\''

let octave_to_string = octave_to_lilypond_string (* FIXME *)

let octave_of_string = function
  | "" -> 0
  | str ->
    let chr = str.[0] in
    if String.exists ((<>) chr) str then
      failwith "Common.Music.octave_of_string";
    match chr with
    | '\'' -> String.length str
    | ',' -> -(String.length str)
    | _ -> failwith "Common.Music.octave_of_string"

type t = {
  note: Note.t;
  alteration: alteration;
  octave: octave
}
[@@deriving eq, show {with_path = false}, qcheck2]

let make note alteration octave = {note; alteration; octave}

let note pitch = pitch.note
let alteration pitch = pitch.alteration
let octave pitch = pitch.octave

let with_octave update pitch = {pitch with octave = update pitch.octave}

let c0 = make C Natural 0

let to_string pitch =
  Note.to_string pitch.note ^
  alteration_to_string pitch.alteration ^
  octave_to_string pitch.octave

let to_pretty_string pitch =
  Note.to_string pitch.note ^
  alteration_to_pretty_string pitch.alteration ^
  octave_to_string pitch.octave

let to_lilypond_string pitch =
  Note.to_lilypond_string pitch.note ^
  alteration_to_lilypond_string pitch.alteration ^
  octave_to_lilypond_string pitch.octave

let of_string = function
  | "" -> failwith "Common.Music.Pitch.of_string"
  | s ->
    let note_alteration_str, octave_str =
      (* FIXME: Dirty as fuck *)
      match String.index_opt s ',' with
      | Some i ->
        (String.sub s 0 i, String.sub s i (String.length s - i))
      | None ->
        match String.index_opt s '\'' with
        | Some i ->
          (String.sub s 0 i, String.sub s i (String.length s - i))
        | None ->
          (s, "")
    in
    let note_char = note_alteration_str.[0] in
    let alteration_str = String.sub note_alteration_str 1 (String.length note_alteration_str - 1) in
    {
      note = Note.of_char note_char;
      alteration = alteration_of_string alteration_str;
      octave = octave_of_string octave_str
    }

let to_yojson = Utils.to_yojson__of__to_string to_string
let of_yojson = Utils.of_yojson__of__of_string of_string "Common.Music.Pitch.of_yojson"

let to_int pitch =
  (
    match note pitch with
    | C -> 0
    | D -> 2
    | E -> 4
    | F -> 5
    | G -> 7
    | A -> 9
    | B -> 11
  ) +
    (
      match alteration pitch with
      | Flat -> -1
      | Natural -> 0
      | Sharp -> +1
    ) +
    (octave pitch * 12)

let of_int =
  let of_int = [|
    make C Natural;
    make C Sharp;
    make D Natural;
    make D Sharp;
    make E Natural;
    make F Natural;
    make F Sharp;
    make G Natural;
    make G Sharp;
    make A Natural;
    make A Sharp;
    make B Natural;
  |]
  in
  fun n ->
    of_int.(pmod n 12) (pdiv n 12)

let%test _ = of_int 0 = make C Natural 0
let%test _ = of_int 1 = make C Sharp 0
let%test _ = of_int 12 = make C Natural 1
let%test _ = of_int 23 = make B Natural 1
let%test _ = of_int (-1) = make B Natural (-1)
let%test _ = of_int (-24) = make C Natural (-2)
let%test _ = of_int (-43) = make F Natural (-4)
let%test "of_int % to_int = id" =
  List.for_all (fun n -> to_int (of_int n) = n) (List.init 50 (fun i -> (i - 25)))

(** Variant of {!make} where the target of the transposition is taken relatively
    to the source, that is the note is the closest to the source's note. For
    instance, [make ~from:"A" ~to_:"G"] will be a transposition of +10
    semitones, while [make' ~from:"A" ~to_relative:"G"] will be a transposition
    of -2 semitones. *)
let relative_to ~reference pitch_relative =
  (* forget about octaves, for now, and find the note that is closest to the
     reference note *)
  let closest =
    let reference_0 = to_int @@ with_octave (const 0) reference in
    let candidate n = with_octave (const n) pitch_relative in
    let diff n = to_int (candidate n) - reference_0 in
    match diff 0 with
    | n when n < 0 && abs (diff 1) < abs n -> candidate 1
    | n when n > 0 && abs (diff (-1)) < abs n -> candidate (-1)
    | _ -> candidate 0
  in
  (* now we can add the octaves back *)
  let reference_octave = octave reference in
  let relative_octave = octave pitch_relative in
  with_octave ((+) (reference_octave + relative_octave)) closest

let%test _ = relative_to ~reference: (make C Natural 7) (make B Flat (-1)) = make B Flat 5

let diff p1 p2 = to_int p1 - to_int p2

let add p1 n = of_int (to_int p1 + n)

let%test _ = add c0 0 = c0
let%test _ = add c0 1 = make C Sharp 0
let%test _ = add c0 2 = make D Natural 0
let%test _ = add c0 15 = make D Sharp 1
let%test _ = add c0 (-1) = make B Natural (-1)
let%test _ = add c0 (-2) = make A Sharp (-1)
