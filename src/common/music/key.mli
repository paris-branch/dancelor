(** {1 Key} *)

type t =
  {pitch: Pitch.t; mode: Mode.t}
[@@deriving eq, show, yojson]

val make : Pitch.t -> Mode.t -> t
val pitch : t -> Pitch.t
val mode : t -> Mode.t

val with_pitch : (Pitch.t -> Pitch.t) -> t -> t

val to_string : t -> string
(** eg. C  D#m   Eb *)

val to_pretty_string : t -> string
(** eg. C  D♯m   E♭ *)

val of_string : string -> t
val of_string_opt : string -> t option
