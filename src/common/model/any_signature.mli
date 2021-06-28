open Nes

type t = Any.t

module Type = Any.Type

val type_of : t -> Type.t

val search :
  (* ?filter:VersionFilter.t -> *)
  ?pagination:Pagination.t ->
  ?threshold:float ->
  ?except:Type.t list ->
  string ->
  t Score.t list Lwt.t
