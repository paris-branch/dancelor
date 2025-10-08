(** {1 Note} *)

type t =
  A | B | C | D | E | F | G
[@@deriving eq, show]

val to_char : t -> char
val to_string : t -> string
val to_lilypond_string : t -> string

(** Raises {!Failure}. *)
val of_char : char -> t

(** For use in property-based testing. *)
val gen : t QCheck2.Gen.t
