open Nes

type page = BookCore.page =
  | Version       of VersionCore.t * VersionParameters.t
  | Set           of     SetCore.t * SetParameters.t
  | InlineSet     of     SetCore.t * SetParameters.t

type t = BookCore.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val title : t -> string Lwt.t
val subtitle : t -> string Lwt.t
val short_title : t -> string Lwt.t
val date : t -> Date.t Lwt.t
val contents : t -> page list Lwt.t
val remark : t -> string Lwt.t

val contains_set : SetCore.t Slug.t -> t -> bool
val compare : t -> t -> int

(* {2 Warnings} *)

type warning = BookCore.warning =
  | Empty
  | DuplicateSet of SetCore.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of TuneCore.t * SetCore.t option list

type warnings = warning list

val warnings : t -> warnings Lwt.t

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  BookFilter.t ->
  t Score.t list Lwt.t
