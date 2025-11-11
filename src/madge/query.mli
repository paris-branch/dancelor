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

(** {2 Creating a Query} *)

val singleton : key -> Yojson.Safe.t -> t

(** {2 Importing and Exporting} *)

val to_list : t -> (string * Yojson.Safe.t) list
val to_strings : t -> (string * string list) list

val from_uri : Uri.t -> t option
val from_body : string -> t

val add : key -> Yojson.Safe.t -> t -> t
