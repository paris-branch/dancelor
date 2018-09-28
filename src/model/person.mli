(** {1 Person}

   Abstract representation of a physical or moral person. This can be
   a human being, a band, etc. This can be a dance deviser, a music
   writer, a musician, etc. *)

type t

val create : string -> t
(** [create name] creates a new person going by the given name. *)

val name : t -> string
(** Return the name of a person. *)
