type t [@@deriving eq, ord, show, yojson]

val from_string : string -> t option
val to_string : t -> string
val to_nestring : t -> NesNEString.t
val of_string_exn : string -> t
