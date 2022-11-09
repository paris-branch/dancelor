val _key : string

type t [@@deriving yojson]

val from_string : string -> t
val to_string : t -> string
val to_pretty_string : ?at:bool -> t -> string

val compare : t -> t -> int

(** {2 Helpers to Implement {!NesDate}}

    Do not use them except in there. *)

val internal__to_full : t -> (int * int * int) option
val internal__from_full : int * int * int -> t
