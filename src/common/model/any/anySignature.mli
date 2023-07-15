open Nes

type t = AnyCore.t

module Type = AnyCore.Type

val type_of : t -> Type.t

module Filter : sig
  type t = AnyCore.Filter.t
end

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t

val count :
  ?threshold:float ->
  Filter.t ->
  int Lwt.t
