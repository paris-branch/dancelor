(** {1 Partial Date}

    This module implements the notion of partial date, that is a date from which
    the day or the month may be missing. *)

val _key : string

type t [@@deriving yojson]
(** The type of a partial date. *)

val from_string : string -> t
(** Parses the date from a potentially-truncated ISO 8601 string, eg.
    [2022-11-09], [2022-11] or [2022]. *)

val to_string : t -> string
(** Prints the date as a potentially-truncated ISO 8601 string, eg.
    [2022-11-09], [2022-11] or [2022], *)

val to_pretty_string : ?at: bool -> t -> string
(** Prints the date as a pretty string, eg. [9 November 2022], [November 2022]
    or [2022]. The [?at] argument allows to represent the string “at <date>”,
    eg. [on 9 November 2022], [in November 2022] or [in 2022]. *)

val compare : t -> t -> int
(** Compares two strings. *)

(** {2 Helpers to Implement {!NesDate}}

    Do not use them except in there. *)

val internal__to_full : t -> (int * int * int ) option
val internal__from_full : int * int * int -> t
