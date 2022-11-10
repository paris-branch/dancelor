(** {1 Date}

    This module implements the notion of date with year, month and day. *)

val _key : string

type t [@@deriving yojson]
(** The type of a date. *)

val from_string : string -> t
(** Parses the date from an ISO 8601 string, eg. [2022-11-09]. *)

val to_string : t -> string
(** Prints the date as an ISO 8601 string, eg. [2022-11-09]. *)

val to_pretty_string : ?at:bool -> t -> string
(** Prints the date as a pretty string, eg. [9 November 2022]. The [?at]
    argument allows to represent the string “at <date>”, eg. [on 9 November
    2022]. *)

val now : unit -> t
(** Returns the current date. *)
