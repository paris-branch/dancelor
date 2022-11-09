type t [@@deriving yojson]
type full

val from_string : string -> t
val from_string_full : string -> full

val to_string : t -> string
val to_pretty_string : ?at:bool -> t -> string

val compare : t -> t -> int
