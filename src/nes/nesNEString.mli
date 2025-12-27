(** {1 Non-empty string} *)

type t [@@deriving eq, show, biniou, yojson]
(** A non-empty string. *)

val to_string : t -> string
(** Convert a non-empty string to a regular string. *)

val of_string : string -> t option
(** Convert a string to a non-empty string. *)

val of_string_exn : string -> t
(** Convert a string to a non-empty string, or raise {!Invalid_argument} if the
    string is empty. *)

val map_exn : (string -> string) -> t -> t
(** Map a function over a non-empty string, raise {!Invalid_argument} if the
    result is empty. *)

val of_char : char -> t
