(** {1 Music}

    Types and functions on musical objects. *)

(** {2 Pitch} *)

type note = A | B | C | D | E | F | G
[@@deriving show]

type alteration = Flat | Sharp | Natural
[@@deriving show]

type octave = int
[@@deriving show]

type pitch
[@@deriving show, yojson]

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

type mode = Major | Minor
[@@deriving show]

type key
[@@deriving show, yojson]

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

module Key : Madge_common.SERIALISABLE with type t = key

(** {2 Clef} *)

type clef = Treble | Bass
[@@deriving show, yojson]

val clef_to_string : clef -> string
val clef_to_pretty_string : clef -> string
val clef_to_lilypond_string : clef -> string

val clef_to_symbol : clef -> string
(** Unicode symbol of the clef. *)

val clef_of_string : string -> clef
