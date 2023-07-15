open Nes

type t = VersionCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val tune : t -> TuneCore.t Lwt.t
val bars : t -> int Lwt.t
val key : t -> Music.key Lwt.t
val structure : t -> string Lwt.t
val arranger : t -> CreditCore.t option Lwt.t
val remark : t -> string Lwt.t
val disambiguation : t -> string Lwt.t
val broken : t -> bool Lwt.t

val content : t -> string Lwt.t

val equal : t -> t -> bool Lwt.t

(** {2 Filters} *)

module Filter : sig
  type t = VersionCore.Filter.t

  val is : VersionCore.t -> t

  val tune : TuneCore.Filter.t -> t

  val raw : string -> t TextFormula.or_error

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  tune:TuneCore.t ->
  bars:int ->
  key:Music.key ->
  structure:string ->
  ?arranger:CreditCore.t ->
  ?remark:string ->
  ?disambiguation:string ->
  ?broken:bool ->
  content:string ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  t Score.t list Lwt.t

val count :
  ?threshold:float ->
  Filter.t ->
  int Lwt.t

val mark_broken : t -> unit Lwt.t

val mark_fixed : t -> unit Lwt.t
