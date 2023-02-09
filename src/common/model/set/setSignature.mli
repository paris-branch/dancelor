open Nes

type t = SetCore.t

val slug : t -> t Slug.t Lwt.t
val is_slug_none : t -> bool Lwt.t

val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val deviser : t -> CreditCore.t option Lwt.t
val kind : t -> Kind.dance Lwt.t
val versions_and_parameters : t -> (VersionCore.t * VersionParameters.t) list Lwt.t
val order : t -> SetOrder.t Lwt.t
val instructions : t -> string Lwt.t
val dances : t -> DanceCore.t list Lwt.t
val remark : t -> string Lwt.t

val contains_version : VersionCore.t Slug.t -> t -> bool

val compare : t -> t -> int Lwt.t
val equal : t -> t -> bool Lwt.t

val lilypond_content_cache_key : t -> string Lwt.t

(* {2 Warnings} *)

type warning = SetCore.warning =
  | Empty
  | WrongKind
  | WrongVersionBars of VersionCore.t
  | WrongVersionKind of TuneCore.t
  | DuplicateVersion of TuneCore.t

type warnings = warning list

val warnings : t -> warnings Lwt.t

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_temp :
  name: string ->
  ?deviser: CreditCore.t ->
  kind: Kind.dance ->
  ?versions_and_parameters: (VersionCore.t * VersionParameters.t) list ->
  order: SetOrder.t ->
  ?dances: DanceCore.t list ->
  unit ->
  t Lwt.t

val make_and_save :
  ?status: Status.t ->
  name: string ->
  ?deviser: CreditCore.t ->
  kind: Kind.dance ->
  ?versions_and_parameters: (VersionCore.t * VersionParameters.t) list ->
  order: SetOrder.t ->
  ?dances: DanceCore.t list ->
  unit ->
  t Lwt.t

val delete : t -> unit Lwt.t

val search :
  ?pagination: Pagination.t ->
  ?threshold: float ->
  SetFilter.t ->
  t Score.t list Lwt.t

val count : SetFilter.t -> int Lwt.t
(** Number of sets in the database. *)
