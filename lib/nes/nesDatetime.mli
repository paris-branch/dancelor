(** {1 Datetime} *)

val _key : string

type t [@@deriving yojson]

val of_string : string -> t
(** Parses the date and time from an ISO 8601 string. *)

val to_string : t -> string
(** Prints the date and time as an ISO 8601 string. *)

val now : unit -> t
(** Returns the current date and time. *)
