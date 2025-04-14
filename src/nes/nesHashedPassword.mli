(** {1 Hashed Password}

    High-level utilities to manipulate passwords safely. *)

(** Abstract type of a password. *)
type t

(** Make a password from a clear text. This function will generate a salt
    randomly, and, as such, is not deterministic. *)
val make : clear: string -> t

(** Compare a clear text and a hashed password. *)
val is : clear: string -> t -> bool

(** {2 Utilities} *)

(** Equality of two hashed password. Two hashes of the same password are NOT
    guaranteed to be equal. *)
val equal : t -> t -> bool

(** Debug printing from ppx_deriving_show. *)
val pp : Format.formatter -> t -> unit

(** Serialisation to/from JSON. This will serialise the hash and the salt. *)
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
