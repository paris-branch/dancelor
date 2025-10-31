(** {1 64-Bit Integers} *)

(** {2 Standard Library}

    This module contains everything defined for 64-bit integers by the OCaml
    standard library. For these functions, refer to the official documentation. *)

include module type of Int64

(** {2 Additional Contents} *)

(** {3 Printing Numbers as English Words} *)

val to_english_string : int64 -> string
(** Same as {!to_english_string_unsigned} for signed numbers. For instance,
    [to_english_string (-234L)] returns ["minus two hundred thirty four"]. *)

val to_english_string_times : int64 -> string
(** Same as {!to_english_string_times_unsigned} for signed numbers. For
    instance, [to_english_string_times (-655L)] returns ["minus six hundred fifty
    five times"] and [to_english_string_times_unsigned 2L] returns ["twice"]. *)
