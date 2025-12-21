type t
[@@deriving eq, show, biniou, yojson]

val identity : t

(** Make a transposition from a (potentially negative) number of semitones. *)
val from_semitones : int -> t

val target_pitch : source: Music.Pitch.t -> t -> Music.Pitch.t

val to_semitones : t -> int

val compose : t -> t -> t
