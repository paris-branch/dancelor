type t

val of_string : string -> t
val to_string : t -> string

module S : Madge.STRINGABLE with type t = t
