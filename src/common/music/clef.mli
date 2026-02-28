type t =
  Treble | Alto | Tenor | Bass
[@@deriving eq, show, yojson]

val to_string : t -> string

val to_symbol : t -> string
(** Unicode symbol of the clef. *)

val of_string : string -> t
