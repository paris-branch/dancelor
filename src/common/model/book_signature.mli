open Nes

type page = Book.page =
  | Version       of Version.t * VersionParameters.t
  | Set           of     Set.t * SetParameters.t
  | InlineSet     of     Set.t * SetParameters.t

type t = Book.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val title : t -> string Lwt.t
val subtitle : t -> string Lwt.t
val date : t -> Date.t Lwt.t
val contents : t -> page list Lwt.t
val remark : t -> string Lwt.t

val contains_set : Set.t Slug.t -> t -> bool
val compare : t -> t -> int

(* {2 Warnings} *)

type warning = Book.warning =
  | Empty
  | DuplicateSet of Set.t (* FIXME: duplicate dance? *)
  | DuplicateVersion of Tune.t

type warnings = warning list

val warnings : t -> warnings Lwt.t

(** {2 Filter} *)

module Filter : sig
  type t = Book.Filter.t =
    | Is of Book.t
    | ExistsVersion of Version.Filter.t
    | ForallVersions of Version.Filter.t

  val accepts : t -> Book.t -> bool Lwt.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val all :
  ?filter:Filter.t ->
  ?pagination:Pagination.t ->
  unit -> t list Lwt.t

val search :
  ?filter:Filter.t ->
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t