type t = Yojson.Safe.t

val add_field : string -> t -> t -> t
val add_fields : (string * t ) list -> t -> t

val remove_field : string -> t -> t

val map_field : string -> (t -> t) -> t -> t

val from_string : string -> t
val to_string : t -> string

val find_opt : string list -> t -> t option
val find : string list -> t -> t

val get : k: (t -> 'a option) -> string list -> t -> 'a
val get_or : k: (t -> 'a option) -> default: 'a -> string list -> t -> 'a
val get_opt : k: (t -> 'a option) -> string list -> t -> 'a option

val string : t -> string option
val int : t -> int option
val slug : t -> 'a NesSlug.t option
val strings : t -> string list option
val list : (t -> 'a) -> t -> 'a list option
