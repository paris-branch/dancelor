type t
[@@deriving eq, show, yojson]

val identity : t

(** Make a transposition from a (potentially negative) number of semitones. *)
val from_semitones : int -> t

(** Make a transposition from a source and a target pitch. *)
val from_pitches : source: Music.Pitch.t -> target: Music.Pitch.t -> t

(** Variant of {!from_pitches} where the target of the transposition is taken
    relatively to the source. See {!Music.Pitch.t_relative_to}. *)
val from_pitches' : source: Music.Pitch.t -> target_relative: Music.Pitch.t -> t

val target_pitch : source: Music.Pitch.t -> t -> Music.Pitch.t

val to_semitones : t -> int

val compose : t -> t -> t
