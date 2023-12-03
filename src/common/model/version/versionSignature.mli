open Nes

type t = VersionCore.t



val slug : t -> t Slug.t
val status : t -> Status.t
val tune : t -> TuneCore.t Lwt.t
val bars : t -> int
val key : t -> Music.key
val structure : t -> string
val sources : t -> string list
val arranger : t -> PersonCore.t option Lwt.t
val remark : t -> string
val disambiguation : t -> string
val broken : t -> bool
val modified_at : t -> Datetime.t
val created_at  : t -> Datetime.t

val content : t -> string Lwt.t

val kind : t -> Kind.Version.t Lwt.t
(** Convenient wrapper around {!bars} and {!Tune.kind}. *)

val name : t -> string Lwt.t
(** Convenient wrapper around {!tune} and {!Tune.name}. *)

val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type t = VersionCore.Filter.t

  val accepts : t -> VersionCore.t -> float Lwt.t

  val is : VersionCore.t -> t
  val tuneIs : TuneCore.t -> t
  val tune : TuneCore.Filter.t -> t
  val broken : t
  val kind : Kind.Version.Filter.t -> t
  val key : Music.Key.t -> t

  val raw : string -> t TextFormula.or_error
  val nullary_text_predicates : (string * t) list
  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
  val from_string : ?filename:string -> string -> t TextFormula.or_error
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  tune:TuneCore.t ->
  bars:int ->
  key:Music.key ->
  structure:string ->
  ?arranger:PersonCore.t ->
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
  (int * t Score.t list) Lwt.t
(** [search ?pagination ?threshold filter] returns the list of all the versions
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of versions. The second element of the pair
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

val mark_broken : t -> unit Lwt.t

val mark_fixed : t -> unit Lwt.t
