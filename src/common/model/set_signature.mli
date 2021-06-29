open Nes

type t = Set.t

val slug : t -> t Slug.t Lwt.t
val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val deviser : t -> Credit.t option Lwt.t
val kind : t -> Kind.dance Lwt.t
val versions_and_parameters : t -> (Version.t * VersionParameters.t) list Lwt.t
val instructions : t -> string Lwt.t
val dances : t -> Dance.t list Lwt.t
val remark : t -> string Lwt.t

val contains_version : Version.t Slug.t -> t -> bool

(* {2 Warnings} *)

type warning = Set.warning =
  | Empty
  | WrongKind
  | WrongVersionBars of Version.t
  | WrongVersionKind of Tune.t
  | DuplicateVersion of Tune.t

type warnings = warning list

val warnings : t -> warnings Lwt.t

(** {2 Filter} *)

module Filter : sig
  type t = Set.Filter.t =
    | Is of Set.t
    | Deviser of Credit.Filter.t
    | DeviserIsDefined
    | ExistsVersion of Version.Filter.t
    | ForallVersions of Version.Filter.t

  val accepts : t -> Set.t -> bool Lwt.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val all :
  ?filter:Filter.t ->
  ?pagination:Pagination.t ->
  unit -> t list Lwt.t

val make_temp :
  name:string ->
  ?deviser:Credit.t ->
  kind:Kind.dance ->
  ?versions_and_parameters:(Version.t * VersionParameters.t) list ->
  ?dances:Dance.t list ->
  unit -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  ?deviser:Credit.t ->
  kind:Kind.dance ->
  ?versions_and_parameters:(Version.t * VersionParameters.t) list ->
  ?dances:Dance.t list ->
  unit -> t Lwt.t

val delete : t -> unit Lwt.t

val search :
  ?filter:Filter.t ->
  ?pagination:Pagination.t ->
  ?threshold:float ->
  string ->
  t Score.t list Lwt.t

val count: unit -> int Lwt.t
(** Number of sets in the database. *)
