(** {1 Pitch} *)

type alteration =
  Flat | Sharp | Natural

type octave = int

type t
[@@deriving eq, show, biniou, yojson]

val make : Note.t -> alteration -> octave -> t

val note : t -> Note.t

val to_string : t -> string
val to_pretty_string : t -> string
val to_lilypond_string : t -> string

val of_string : string -> t

(** Difference between two pitches as a number of semitones. *)
val diff : t -> t -> int

(** Add a number of semitones to a pitch. *)
val add : t -> int -> t

(** For use in property-based testing. *)
val gen : t QCheck2.Gen.t
