open Nes

type t = AnyCore.t

module Type : sig
  type t = AnyCore.Type.t
end

val type_of : t -> Type.t

module Filter : sig
  type t = AnyCore.Filter.t

  val accepts : t -> AnyCore.t -> float Lwt.t
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
