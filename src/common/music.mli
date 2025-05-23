(** {1 Music}

    Types and functions on musical objects. *)

(** {2 Pitch} *)

type note =
  A | B | C | D | E | F | G
[@@deriving eq, show]

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

val pitch_c : pitch

(* val pitch_to_string : pitch -> string *)
val pitch_to_pretty_string : pitch -> string
val pitch_to_lilypond_string : pitch -> string

(* val pitch_of_string : string -> pitch *)

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

val key_to_lilypond_string : key -> string
(** eg. c dis:m ees *)

val key_to_safe_string : key -> string
(** eg. c  dism ees *)

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
