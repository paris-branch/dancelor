(** {1 Results} *)

(** {2 Standard Library}

    This module contains everything defined for lists by the OCaml standard
    library. For these functions, refer to the official documentation. *)

include module type of Stdlib.Result

(** {2 Additional Contents} *)

val of_string_nonempty : empty: string -> string -> (string, string) result
(** Maps [""] to [Error empty] and other strings to [Ok]. *)
