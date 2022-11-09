val _key : string

type t

val to_string : t -> string
val to_pretty_string : ?at:bool -> t -> string

val from_string : string -> t
