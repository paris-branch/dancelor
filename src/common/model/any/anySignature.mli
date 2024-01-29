open Nes

type t = AnyCore.t

val person : PersonCore.t -> t
val dance : DanceCore.t -> t
val book : BookCore.t -> t
val set : SetCore.t -> t
val tune : TuneCore.t -> t
val version : VersionCore.t -> t

val equal : t -> t -> bool

val name : t -> string Lwt.t
(** Finds a name to give to the element, no matter what it is. *)

module Type : sig
  type t = [%import: AnyCore.Type.t]

  val all : t list
  val to_string : t -> string
end

val type_of : t -> Type.t

module Filter : sig
  type predicate = [%import: AnyCore.Filter.predicate]
  type t = [%import: AnyCore.Filter.t]

  val type_ : Type.t -> t

  val accepts : t -> AnyCore.t -> float Lwt.t

  val from_string : string -> (t, string list) result
  val from_string_exn : string -> t

  val possible_types : t -> Type.t list
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
