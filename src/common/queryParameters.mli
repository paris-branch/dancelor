(** {1 Query Parameters} *)

(** The abstract type of a query *)
type t

type key = string

(** Get the parameter whose key is given. Depending on the version, return an
   option or raises [Not_found]. *)
val get : key -> t -> Yojson.Safe.t option
val get_exn : key -> t -> Yojson.Safe.t

(** {2 Importing and Exporting} *)

val to_list : t -> (string * Yojson.Safe.t) list

val from_uri : Uri.t -> t
val from_body : Cohttp_lwt.Body.t -> t Lwt.t

(** Append query parameters together. The resulting query parameters are taken
   from the input ones with priority [high] and [low]. *)
val append : high:t -> low:t -> t
