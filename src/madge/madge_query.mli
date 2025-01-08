(** {1 Query Parameters} *)

(** The abstract type of a query *)
type t

type key = string

val empty : t

val is_empty : t -> bool

(** Get the parameter whose key is given. *)
val get : key -> t -> Yojson.Safe.t option

(** Get the parameter whose key is given, and return the query without that
    parameter. *)
val extract : key -> t -> (Yojson.Safe.t * t) option

(** Exception raised when a parameter is expected of a certain type (first
    string) but has another one (second string). *)
exception WrongType of string * string

(** Get the parameter whose key is given and apply a casting function to it. *)
val get_ : key -> (Yojson.Safe.t -> ('value, 'error) result) -> t -> 'value option

(** Get the parameter whose key is given, converted to the right type. Return
    [None] if the parameter is not found. Raises {!WrongType} if the parameter is
    found but has the wrong type. *)
val get_string : key -> t -> string option

(** {2 Creating a Query} *)

val singleton : key -> Yojson.Safe.t -> t

(** {2 Importing and Exporting} *)

val to_list : t -> (string * Yojson.Safe.t) list
val to_strings : t -> (string * string list) list

val from_uri : Uri.t -> t
val from_body : Cohttp_lwt.Body.t -> t Lwt.t

val add : key -> Yojson.Safe.t -> t -> t

(** Append query parameters together. The resulting query parameters are taken
    from the input ones with priority [high] and [low]. *)
val append : high: t -> low: t -> t
