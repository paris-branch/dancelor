open Nes

type t = Any.t

module Type = Any.Type

val type_of : t -> Type.t

module Filter : sig
  type t = Any.Filter.t =
    | Is of Any.t
    | TypeIs of Type.t

  val accepts : t -> Any.t -> bool Lwt.t
end

val search :
  ?filter:Filter.t Formula.t ->
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t
