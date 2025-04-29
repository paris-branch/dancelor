(** {1 Hashed secret}

    High-level utilities to manipulate hashed values safely, eg. tokens or
    passwords. *)

include module type of Nes.HashedSecret

val make : clear: string -> t
(** Make a password from a clear text. This function will generate a salt
    randomly, and, as such, is not deterministic. *)

val is : clear: string -> t -> bool
(** Compare a clear text and a hashed password. *)
