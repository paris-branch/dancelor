open Nes
open Dancelor_common_database

type t = SetCore.t

val make :
  name: string ->
  ?conceptors: PersonCore.t Entry.t list ->
  kind: Kind.Dance.t ->
  ?contents: (VersionCore.t Entry.t * VersionParameters.t) list ->
  order: SetOrder.t ->
  ?dances: DanceCore.t Entry.t list ->
  unit ->
  t

(* FIXME: remove? *)
val is_slug_none : t Entry.t -> bool

val name : t Entry.t -> string
val conceptors : t Entry.t -> PersonCore.t Entry.t list Lwt.t
val kind : t Entry.t -> Kind.Dance.t
val contents : t Entry.t -> (VersionCore.t Entry.t * VersionParameters.t) list Lwt.t
val order : t Entry.t -> SetOrder.t
val instructions : t Entry.t -> string
val dances : t Entry.t -> DanceCore.t Entry.t list Lwt.t
val remark : t Entry.t -> string

val contains_version : VersionCore.t Slug.t -> t Entry.t -> bool
(** REVIEW: This really takes a slug? *)

val find_context : int -> t Entry.t -> VersionCore.t Entry.t List.context option Lwt.t
(** Given an indice and a set, find the context around that indice in the
    set. *)

val compare : t Entry.t -> t Entry.t -> int
val equal : t Entry.t -> t Entry.t -> bool

val lilypond_content_cache_key : t Entry.t -> string Lwt.t

(* {2 Warnings} *)

type warning = SetCore.warning =
  | Empty
  | WrongKind
  | WrongVersionBars of VersionCore.t Entry.t
  | WrongVersionKind of TuneCore.t Entry.t
  | DuplicateVersion of TuneCore.t Entry.t

type warnings = warning list

val warnings : t Entry.t -> warnings Lwt.t

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: SetCore.Filter.predicate]
  type t = [%import: SetCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> SetCore.t Entry.t -> float Lwt.t

  val is : SetCore.t Entry.t -> predicate
  val is' : SetCore.t Entry.t -> t

  val existsVersion : VersionCore.Filter.t -> predicate
  val existsVersion' : VersionCore.Filter.t -> t

  val existsConceptor : PersonCore.Filter.t -> predicate
  val existsConceptor' : PersonCore.Filter.t -> t

  val kind : KindDance.Filter.t -> predicate
  val kind' : KindDance.Filter.t -> t

  val memVersion : VersionCore.t Entry.t -> predicate
  val memVersion' : VersionCore.t Entry.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Entry.t Lwt.t

val save :
  ?status: Dancelor_common_database.Status.t ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  t ->
  t Entry.t Lwt.t

val delete : t Entry.t -> unit Lwt.t

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t Entry.t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the sets
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of sets. The second element of the pair
    is a slice of the list, taken as per the [slice] (if any). *)

val search' :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  t Entry.t list Lwt.t
(** Like {!search} but returns only the list. *)

val count :
  ?threshold: float ->
  Filter.t ->
  int Lwt.t
(** Like {!search} but returns only the number of items. *)
