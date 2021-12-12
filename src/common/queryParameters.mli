(** {1 Query Parameters} *)

(** The abstract type of a query *)
type t

type key = string

(** Get the parameter whose key is given. Depending on the version, return an
   option or raises [Not_found]. *)
val get : key -> t -> Yojson.Safe.t option
val get_exn : key -> t -> Yojson.Safe.t

(** {Building} *)

val to_list : t -> (string * Yojson.Safe.t) list
val from_list : (string * Yojson.Safe.t) list -> t
