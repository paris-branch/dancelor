(** {1 Mode} *)

type t = Major | Minor [@@deriving eq, show]

val to_string : t -> string
val to_lilypond_string : t -> string

(** For use in property-based testing. *)
val gen : t QCheck2.Gen.t
