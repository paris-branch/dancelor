(** {1 Person}

    This module represents a “person”, in the wide definition. A person can be
    an actual human person (eg. “William Marshall”), a group of people (eg. “The
    Craigellachie Band”) or an abstract notion (eg. “Traditional” or “RSCDS”).
    This is a notion similar to that of the SCDDB. *)

open Nes
open Dancelor_common_database
open Dancelor_common_model_utils
open Dancelor_common_model_core

type t = Person.t
(** Abstract type for a person. *)

val make : name: string -> ?scddb_id: int -> unit -> t

(** {2 Field getters} *)

val name : t Entry.t -> string
val scddb_id : t Entry.t -> int option

val is_trad : t Entry.t -> bool

val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: Dancelor_common_model_filter.Person.predicate]
  type t = [%import: Dancelor_common_model_filter.Person.t]
  [@@deriving eq, show]

  val accepts : t -> Person.t Entry.t -> float Lwt.t

  val is : Person.t Entry.t -> predicate
  val is' : Person.t Entry.t -> t

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

val create : t -> t Entry.t Lwt.t
(** Create a new database entry for the given person. *)

val update : t Slug.t -> t -> t Entry.t Lwt.t
(** Update an existing database entry with the given person. *)

val save : ?slug: t Slug.t -> t -> t Entry.t Lwt.t
(** Either {!create} or {!update}. *)

val search : Slice.t -> Filter.t -> (int * t Entry.t list) Lwt.t
(** Returns the list of all the persons that match the filter with a score
    higher than the hardcoded threshold. The first element of the pair is the
    number of persons. The second element of the pair is a slice of the list,
    taken as per the slice. *)

val search' : Filter.t -> t Entry.t list Lwt.t
(** Like {!search} but returns only the list of all values. *)

val count : Filter.t -> int Lwt.t
(** Like {!search} but returns only the number of items. *)
