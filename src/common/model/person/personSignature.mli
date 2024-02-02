(** {1 Person}

    This module represents a “person”, in the wide definition. A person can be
    an actual human person (eg. “William Marshall”), a group of people (eg. “The
    Craigellachie Band”) or an abstract notion (eg. “Traditional” or “RSCDS”).
    This is a notion similar to that of the SCDDB. *)

open Nes

type t = PersonCore.t
(** Abstract type for a person. *)

(** {2 Field getters} *)

val slug : t -> t Slug.t
val status : t -> Status.t
val name : t -> string
val scddb_id : t -> int option
val modified_at : t -> Datetime.t
val created_at : t -> Datetime.t

val is_trad : t -> bool

val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: PersonCore.Filter.predicate]
  type t = [%import: PersonCore.Filter.t]

  val accepts : t -> PersonCore.t -> float Lwt.t

  val is : PersonCore.t -> t

  val text_formula_converter : predicate TextFormulaConverter.t
  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename:string -> string -> (t, string) Result.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t
(** Look up a person in the database given its slug. On the client-side, this
    involves an API call. *)

val make_and_save :
  ?status:Status.t ->
  name:string ->
  ?scddb_id:int ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  (int * t Score.t list) Lwt.t
(** [search ?pagination ?threshold filter] returns the list of all the persons
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of persons. The second element of the pair
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
