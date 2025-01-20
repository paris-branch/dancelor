open Nes

type t = VersionCore.t

val tune : t -> TuneCore.t Lwt.t
val bars : t -> int
val key : t -> Music.key
val structure : t -> string
val sources : t -> string list
val arrangers : t -> PersonCore.t list Lwt.t
val remark : t -> string
val disambiguation : t -> string

val content : t -> string Lwt.t

val kind : t -> Kind.Version.t Lwt.t
(** Convenient wrapper around {!bars} and {!Tune.kind}. *)

val name : t -> string Lwt.t
(** Convenient wrapper around {!tune} and {!Tune.name}. *)

val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: VersionCore.Filter.predicate]
  type t = [%import: VersionCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> VersionCore.t -> float Lwt.t

  val is : VersionCore.t -> predicate
  val is' : VersionCore.t -> t

  val tuneIs : TuneCore.t -> predicate
  val tuneIs' : TuneCore.t -> t

  val tune : TuneCore.Filter.t -> predicate
  val tune' : TuneCore.Filter.t -> t

  val kind : Kind.Version.Filter.t -> predicate
  val kind' : Kind.Version.Filter.t -> t

  val key : Music.Key.t -> predicate
  val key' : Music.Key.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val save :
  ?status: Dancelor_common_database.Status.t ->
  tune: TuneCore.t ->
  bars: int ->
  key: Music.key ->
  structure: string ->
  ?arrangers: PersonCore.t list ->
  ?remark: string ->
  ?disambiguation: string ->
  content: string ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Lwt.t

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the versions
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of versions. The second element of the pair
    is a slice of the list, taken as per the [slice] (if any). *)

val search' :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  t list Lwt.t
(** Like {!search} but returns only the list. *)

val count :
  ?threshold: float ->
  Filter.t ->
  int Lwt.t
(** Like {!search} but returns only the number of items. *)
