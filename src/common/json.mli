type value = Ezjsonm.value
type t = [ `O of (string * value) list ]

val of_value : value -> t
val to_value : t -> value

val add_field : string -> value -> value -> value
val add_fields : (string * value) list -> value -> value

val map_field : string -> (value -> value) -> value -> value

val from_string : string -> t
val to_string : t -> string

val find_opt : string list -> value -> value option
val get_opt : k:(value -> 'a) -> string list -> value -> 'a option
val get : k:(value -> 'a) -> string list -> value -> 'a

val string : value -> string
val int : value -> int
val slug : value -> Slug.t
val strings : value -> string list
