type t

val of_string : string -> t
val to_string : t -> string

val add_suffix : t -> string -> t

module S : Madge.STRINGABLE with type t = t
