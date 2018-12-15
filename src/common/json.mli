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

val get : k:(value -> 'a option) -> string list -> t -> 'a
val get_or : k:(value -> 'a option) -> default:'a -> string list -> t -> 'a
val get_opt : k:(value -> 'a option) -> string list -> t -> 'a option

val string : value -> string option
val int : value -> int option
val slug : value -> Slug.t option
val strings : value -> string list option
val list : (value -> 'a option) -> value -> 'a list option
