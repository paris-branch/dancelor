(** {1 Integers} *)

(** {2 Standard Library}

    This module contains everything defined for integers by the OCaml standard
    library. For these functions, refer to the official documentation. *)

include module type of Stdlib.Int

(** {2 Additional Contents} *)

(** {3 Printing Numbers as English Words}

    For compatibility with all platforms, including 32-bit platforms or JS
    extraction, these functions are defined in the 64-bit integer modules
    {!NesInt64}. Refer to this module for the documentation. *)

val to_english_string : int -> string
(** See {!NesInt64.to_english_string}. *)

val to_english_string_times : int -> string
(** See {!NesInt64.to_english_string_times}. *)
