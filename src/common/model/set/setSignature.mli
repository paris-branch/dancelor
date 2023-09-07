open Nes

type t = SetCore.t

val slug : t -> t Slug.t Lwt.t
val is_slug_none : t -> bool Lwt.t

val status : t -> Status.t Lwt.t
val name : t -> string Lwt.t
val deviser : t -> CreditCore.t option Lwt.t
val kind : t -> Kind.Dance.t Lwt.t
val versions_and_parameters : t -> (VersionCore.t * VersionParameters.t) list Lwt.t
val order : t -> SetOrder.t Lwt.t
val instructions : t -> string Lwt.t
val dances : t -> DanceCore.t list Lwt.t
val remark : t -> string Lwt.t
val modified_at : t -> Datetime.t Lwt.t
val created_at : t -> Datetime.t Lwt.t

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

(** {2 Filters} *)

module Filter: sig
  type t = SetCore.Filter.t

  val accepts : t -> SetCore.t -> float Lwt.t

  val is : SetCore.t -> t
  val existsVersion : VersionCore.Filter.t -> t
  val deviser : CreditCore.Filter.t -> t
  val memVersion : VersionCore.t -> t

  val raw : string -> t TextFormula.or_error
  val nullary_text_predicates : (string * t) list
  val unary_text_predicates : (string * (TextFormula.t -> t TextFormula.or_error)) list

  val from_text_formula : TextFormula.t -> t TextFormula.or_error
  val from_string : ?filename: string -> string -> t TextFormula.or_error
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_temp :
  name: string ->
  ?deviser: CreditCore.t ->
  kind: Kind.Dance.t ->
  ?versions_and_parameters: (VersionCore.t * VersionParameters.t) list ->
  order: SetOrder.t ->
  ?dances: DanceCore.t list ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Lwt.t

val make_and_save :
  ?status: Status.t ->
  name: string ->
  ?deviser: CreditCore.t ->
  kind: Kind.Dance.t ->
  ?versions_and_parameters: (VersionCore.t * VersionParameters.t) list ->
  order: SetOrder.t ->
  ?dances: DanceCore.t list ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Lwt.t

val delete : t -> unit Lwt.t

val search :
  ?pagination: Pagination.t ->
  ?threshold: float ->
  Filter.t ->
  t Score.t list Lwt.t

val count : Filter.t -> int Lwt.t
(** Number of sets in the database. *)
