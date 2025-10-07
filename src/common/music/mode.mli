(** {1 Mode} *)

type t = Major | Minor [@@deriving eq, show]

val to_string : t -> string
val to_lilypond_string : t -> string
