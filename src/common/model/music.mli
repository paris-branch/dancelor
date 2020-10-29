(** {1 Music}

    Types and functions on musical objects. *)

(** {2 Pitch} *)

type note = A | B | C | D | E | F | G
type alteration = Flat | Sharp | Natural
type octave = int

type pitch [@@deriving yojson]

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

type key [@@deriving yojson]

val make_key : pitch -> mode -> key
val key_pitch : key -> pitch

val key_to_string : key -> string          (** eg. C  D#m   Eb *)
val key_to_pretty_string : key -> string   (** eg. C  D♯m   E♭ *)
val key_to_lilypond_string : key -> string (** eg. c dis:m ees *)
val key_to_safe_string : key -> string     (** eg. c  dism ees *)

val key_of_string : string -> key

(** {2 Clef} *)

type clef = Treble | Bass [@@deriving yojson]

val clef_to_string : clef -> string
val clef_to_pretty_string : clef -> string
val clef_to_lilypond_string : clef -> string

val clef_to_symbol : clef -> string
(** Unicode symbol of the clef. *)

val clef_of_string : string -> clef
