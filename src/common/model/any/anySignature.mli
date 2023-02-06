open Nes

type t = AnyCore.t

module Type = AnyCore.Type

val type_of : t -> Type.t

val search :
  ?pagination: Pagination.t ->
  ?threshold: float ->
  AnyFilter.t ->
  t Score.t list Lwt.t

val count :
  ?threshold: float ->
  AnyFilter.t ->
  int Lwt.t
