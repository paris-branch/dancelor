(** {1 Datetime} *)

type t [@@deriving yojson]

val of_string : string -> t
(** Parses the date and time from an ISO 8601 string. *)

val to_string : t -> string
(** Prints the date and time as an ISO 8601 string. *)

val pp : Format.formatter -> t -> unit
(** See {!to_string}. *)

val now : unit -> t
(** Returns the current date and time. *)

val plus : float -> t -> t
(** Adds the given number of seconds to a datetime, producing a new one. *)

val diff : t -> t -> float
(** Number of seconds between two datetimes. *)

val make_in_the_future : float -> t
(** Make a datetime in the future by the given number of seconds. *)

val in_the_past : t -> bool
(** Whether the given datetime is in the past. *)
