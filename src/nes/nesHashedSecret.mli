(** {1 Hashed secret}

    High-level utilities to manipulate hashed secrets safely. This can be used
    for passwords and tokens, for instance. *)

type t [@@deriving biniou, yojson]
(** Abstract type of a hashed secret. *)

(** {2 Utilities} *)

val equal : t -> t -> bool
(** Equality of two hashed secrets. Two hashes of the same secret are NOT
    guaranteed to be equal. *)

val pp : Format.formatter -> t -> unit
(** Debug printing from ppx_deriving_show. *)

(**/**)
val unsafe_of_string : string -> t
val unsafe_to_string : t -> string
