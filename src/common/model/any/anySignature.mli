open Nes

type t = AnyCore.t

val equal : t -> t -> bool

module Type : sig
  type t = AnyCore.Type.t

  val to_string : t -> string
end

val type_of : t -> Type.t

module Filter : sig
  type t = AnyCore.Filter.t

  val accepts : t -> AnyCore.t -> float Lwt.t

  val from_string : string -> (t, string list) result
  val from_string_exn : string -> t
end

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  (int * t Score.t list) Lwt.t
(** [search ?pagination ?threshold filter] returns the list of all the objects
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of objects. The second element of the pair
    is a slice of the list, taken as per the [pagination] (if any). *)

val search' :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
(** Like {!search} but returns only the list. *)

val count :
  ?threshold:float ->
  Filter.t ->
  int Lwt.t
(** Like {!search} but returns only the number of items. *)
