(** {1 Pitch} *)

type alteration =
  Flat | Sharp | Natural

type octave = int

type t
[@@deriving eq, show, yojson]

val make : Note.t -> alteration -> octave -> t

val note : t -> Note.t
val alteration : t -> alteration
val octave : t -> octave

(** Replace the given octave in pitch. *)
val with_octave : (octave -> octave) -> t -> t

val c0 : t

val to_string : t -> string
val to_pretty_string : t -> string
val to_lilypond_string : t -> string

val of_string : string -> t

(** Difference between two pitches as a number of semitones. *)
val diff : t -> t -> int

(** Add a number of semitones to a pitch. *)
val add : t -> int -> t

(** Interpret the given pitch as relative to a reference. This is the same
    semantics as LilyPond's [\relative]. For instance, [relative_to
    ~reference:"A" "G" = "G,"]. *)
val relative_to : reference: t -> t -> t

(** For use in property-based testing. *)
val gen : t QCheck2.Gen.t
