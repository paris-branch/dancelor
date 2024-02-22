open Nes

type t = SetCore.t

val slug : t -> t Slug.t
val is_slug_none : t -> bool

val status : t -> Status.t
val name : t -> string
val deviser : t -> PersonCore.t option Lwt.t
val kind : t -> Kind.Dance.t
val versions_and_parameters : t -> (VersionCore.t * VersionParameters.t) list Lwt.t
val order : t -> SetOrder.t
val instructions : t -> string
val dances : t -> DanceCore.t list Lwt.t
val remark : t -> string
val modified_at : t -> Datetime.t
val created_at : t -> Datetime.t

val contains_version : VersionCore.t Slug.t -> t -> bool
(** REVIEW: This really takes a slug? *)

val compare : t -> t -> int
val equal : t -> t -> bool

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

module Filter : sig
  type predicate = [%import: SetCore.Filter.predicate]
  type t = [%import: SetCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> SetCore.t -> float Lwt.t

  val is : SetCore.t -> predicate
  val is' : SetCore.t -> t

  val existsVersion : VersionCore.Filter.t -> predicate
  val existsVersion' : VersionCore.Filter.t -> t

  val deviser : PersonCore.Filter.t -> predicate
  val deviser' : PersonCore.Filter.t -> t

  val kind : KindDance.Filter.t -> predicate
  val kind' : KindDance.Filter.t -> t

  val memVersion : VersionCore.t -> predicate
  val memVersion' : VersionCore.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename:string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make :
  ?status:Status.t ->
  ?slug:t Slug.t ->
  name:string ->
  ?deviser:PersonCore.t ->
  kind:Kind.Dance.t ->
  ?versions_and_parameters:(VersionCore.t * VersionParameters.t) list ->
  order:SetOrder.t ->
  ?dances:DanceCore.t list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  ?deviser:PersonCore.t ->
  kind:Kind.Dance.t ->
  ?versions_and_parameters:(VersionCore.t * VersionParameters.t) list ->
  order:SetOrder.t ->
  ?dances:DanceCore.t list ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val delete : t -> unit Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  (int * t Score.t list) Lwt.t
(** [search ?pagination ?threshold filter] returns the list of all the sets
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of sets. The second element of the pair
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
