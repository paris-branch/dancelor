(** {1 Music}

    Types and functions on musical objects. *)

(** {2 Note, alteration, pitch, mode} *)

type note = A | B | C | D | E | F | G
(** Type for notes. *)

type alteration = Flat | Sharp | Natural
(** Type for alterations. *)

type pitch = note * alteration
(** An alteration plus a note gives a pitch. *)

val pitch_to_string : pitch -> string
(** Prints a pitch as a LilyPond-like string. **)

val pitch_to_pretty_string : pitch -> string

type mode = Major | Minor
(** Type for modes. *)

(** {2 Key} *)

type key = pitch * mode [@@deriving yojson]
(** Type for keys. *)

val key_to_string : key -> string
(** Prints a key as a LilyPond-like string. *)

val key_of_string : string -> key
(** Interpret a string as a key. The string must represent a LilyPond-like key. *)

val key_to_safe_string : key -> string
(** Turns a key into a safe string, avoiding any weird character including flat,
   sharp, but also colon for instance. *)

val key_to_pretty_string : key -> string
(** Turns a key into a pretty human-readable string. *)

(** {2 Clef} *)

type clef = Treble | Bass [@@deriving yojson]

val clef_to_string : clef -> string
val clef_of_string : string -> clef
