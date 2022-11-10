open Nes

type t = TuneCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val alternative_names : t -> string list Lwt.t
val kind : t -> Kind.base Lwt.t
val author : t -> CreditCore.t option Lwt.t
val dances : t -> DanceCore.t list Lwt.t
val remark : t -> string Lwt.t
val scddb_id : t -> int option Lwt.t

val compare : t -> t -> int Lwt.t
val equal : t -> t -> bool Lwt.t

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  ?alternative_names:string list ->
  kind:Kind.base ->
  ?author:CreditCore.t ->
  ?dances:DanceCore.t list ->
  ?remark:string ->
  ?scddb_id:int ->
  modified_at:Date.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  TuneFilter.t ->
  t Score.t list Lwt.t
