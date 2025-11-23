type t = {
  melody: string; (** the melody of that part; they must not include clef or time; they may include the key *)
  chords: string; (** the chords of that part; they will be interpreted in LilyPond's [\chordmode] *)
}
[@@deriving eq, yojson, show, fields]

val empty : t

val space : t
(** A space in both melody and chords. *)

val section_break : t
(** [\section\break] in the melody; space in the chords. *)

val fine : t
(** [\fine] in the melody; nothing in the chords. *)

val mark : Part_name.t -> t
(** A part name mark in the melody; nothing in the chords. *)

val concat : t -> t -> t
(** Concatenates melodies together and chords together in both voices. *)

val concat_l : t list -> t
(** Like {!concat} but on a list of voices. *)
