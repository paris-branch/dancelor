type value = Ezjsonm.value
type t = [ `O of (string * value) list ]

val of_value : value -> t
val to_value : t -> value
val on_value : (t -> t) -> value -> value
val to_ezjsonm : t -> Ezjsonm.t

val add_field : string -> value -> t -> t
val add_fields : (string * value) list -> t -> t

val map_field : string -> (value -> value) -> t -> t

val from_string : string -> t
val to_string : t -> string

val find_opt : string list -> t -> value option
val find : string list -> t -> value
val get_opt : k:(value -> 'a) -> string list -> t -> 'a option
val get : k:(value -> 'a) -> string list -> t -> 'a

val string : value -> string
val int : value -> int
val slug : value -> Slug.t
val strings : value -> string list
val list : (value -> 'a) -> value -> 'a list
