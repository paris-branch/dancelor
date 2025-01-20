(** {1 Dance} *)

open Nes
open Dancelor_common_database

type t = DanceCore.t

val make :
  name: string ->
  kind: Kind.Dance.t ->
  ?devisers: PersonCore.t Entry.t list ->
  ?two_chords: bool ->
  ?scddb_id: int ->
  ?disambiguation: string ->
  ?date: PartialDate.t ->
  unit ->
  t

(** {2 Field getters} *)

val name : t Entry.t -> string
val kind : t Entry.t -> Kind.Dance.t
val devisers : t Entry.t -> PersonCore.t Entry.t list Lwt.t
val two_chords : t Entry.t -> bool option
val scddb_id : t Entry.t -> int option
val disambiguation : t Entry.t -> string
val date : t Entry.t -> PartialDate.t option

val equal : t Entry.t -> t Entry.t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: DanceCore.Filter.predicate]
  type t = [%import: DanceCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> DanceCore.t Entry.t -> float Lwt.t

  val is : DanceCore.t Entry.t -> predicate
  val is' : DanceCore.t Entry.t -> t

  val kind : KindDance.Filter.t -> predicate
  val kind' : KindDance.Filter.t -> t

  val existsDeviser : PersonCore.Filter.t -> predicate
  val existsDeviser' : PersonCore.Filter.t -> t

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

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t Entry.t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the dances
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of dances. The second element of the pair
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
