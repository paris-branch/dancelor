(** {1 Music}

    Types and functions on musical objects. *)

(** {2 Pitch} *)

type note =
  A | B | C | D | E | F | G
[@@deriving eq, show]

val note_to_lilypond_string : note -> string

type alteration =
  Flat | Sharp | Natural
[@@deriving eq, show]

type octave = int
[@@deriving eq, show]

type pitch = {
  note: note;
  alteration: alteration;
  octave: octave
}
[@@deriving eq, show, yojson]

val make_pitch : note -> alteration -> octave -> pitch

val pitch_note : pitch -> note
val pitch_alteration : pitch -> alteration
val pitch_octave : pitch -> octave

(** Replace the given octave in pitch. *)
val pitch_with_octave : (octave -> octave) -> pitch -> pitch

val pitch_c0 : pitch

val pitch_to_pretty_string : pitch -> string
val pitch_to_lilypond_string : pitch -> string

(* FIXME: Looks like we should just have a type for a difference between two
   pitches. This would be morally an int, but with proper typing. In fact, maybe
   pitches should also be stored as integers relative to C0? *)

(** Difference between two pitches as a number of semitones. *)
val pitch_diff : pitch -> pitch -> int

(** Add a number of semitones to a pitch. *)
val pitch_add : pitch -> int -> pitch

(** Interpret the given pitch as relative to a reference. This is the same
    semantics as LilyPond's [\relative]. For instance, [pitch_relative_to
    ~reference:"A" "G" = "G,"]. *)
val pitch_relative_to : reference: pitch -> pitch -> pitch

(** {2 Key} *)

type mode =
  Major | Minor
[@@deriving eq, show]

type key =
  {pitch: pitch; mode: mode}
[@@deriving eq, show, yojson]

val make_key : pitch -> mode -> key
val key_pitch : key -> pitch

val key_to_string : key -> string
(** eg. C  D#m   Eb *)

val key_to_pretty_string : key -> string
(** eg. C  D♯m   E♭ *)

val key_of_string : string -> key
val key_of_string_opt : string -> key option

module Key : Madge.JSONABLE with type t = key

(** {2 Clef} *)

type clef =
  Treble | Bass
[@@deriving eq, show, yojson]

val clef_to_string : clef -> string
val clef_to_pretty_string : clef -> string
val clef_to_lilypond_string : clef -> string

val clef_to_symbol : clef -> string
(** Unicode symbol of the clef. *)

val clef_of_string : string -> clef
