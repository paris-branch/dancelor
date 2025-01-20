(** {1 Person}

    This module represents a “person”, in the wide definition. A person can be
    an actual human person (eg. “William Marshall”), a group of people (eg. “The
    Craigellachie Band”) or an abstract notion (eg. “Traditional” or “RSCDS”).
    This is a notion similar to that of the SCDDB. *)

open Nes
open Dancelor_common_database

type t = PersonCore.t
(** Abstract type for a person. *)

(** {2 Field getters} *)

val name : t Entry.t -> string
val scddb_id : t Entry.t -> int option

val is_trad : t Entry.t -> bool

val equal : t Entry.t -> t Entry.t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: PersonCore.Filter.predicate]
  type t = [%import: PersonCore.Filter.t]
  [@@deriving eq, show]

  val accepts : t -> PersonCore.t Entry.t -> float Lwt.t

  val is : PersonCore.t Entry.t -> predicate
  val is' : PersonCore.t Entry.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename: string -> string -> (t, string) Result.t
  val to_string : t -> string

  val optimise : t -> t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Entry.t Lwt.t
(** Look up a person in the database given its slug. On the client-side, this
    involves an API call. *)

val save :
  ?status: Dancelor_common_database.Status.t ->
  name: string ->
  ?scddb_id: int ->
  modified_at: Datetime.t ->
  created_at: Datetime.t ->
  unit ->
  t Entry.t Lwt.t

val search :
  ?slice: Slice.t ->
  ?threshold: float ->
  Filter.t ->
  (int * t Entry.t list) Lwt.t
(** [search ?slice ?threshold filter] returns the list of all the persons
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of persons. The second element of the pair
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
