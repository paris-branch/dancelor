open Nes

type destructured = {
  parts: Voices.t NEList.t;
  transitions: (Part_name.open_ * Part_name.open_ * Voices.t) list;
  default_structure: Structure.t;
}

type t =
  | Monolithic of {lilypond: string; bars: int; structure: Structure.t} (** A tune as a full LilyPond, including clef, key, etc. *)
  | Destructured of destructured (** A tune decomposed as building blocks *)
[@@deriving eq, yojson, show, variants]

val lilypond : Kind.Base.t -> Music.Key.t -> t -> string

val erase_lilypond : t -> t
(** Erase the LilyPond from a version content. This is used to send smaller
    payloads to the Dancelor client. *)
