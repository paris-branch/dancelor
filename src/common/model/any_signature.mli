open Nes

type t = Any.t

module Type = Any.Type

val type_of : t -> Type.t

module Filter : sig
  include module type of Any.Filter
  val accepts : t -> Any.t -> bool Lwt.t
end

val search :
  ?filter:Filter.t Formula.t ->
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t
