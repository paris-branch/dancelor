open Nes

type t = Any.t

module Type = Any.Type

val type_of : t -> Type.t

module Filter : sig
  include module type of Any.Filter
  val accepts : t -> Any.t -> float Lwt.t
end

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t
