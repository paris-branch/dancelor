(** {1 Dance} *)

open Nes

type t = DanceCore.t

(** {2 Field getters} *)

val slug : t -> t Slug.t
val status : t -> Status.t
val name : t -> string
val kind : t -> Kind.Dance.t
val deviser : t -> PersonCore.t option Lwt.t
val two_chords : t -> bool
val scddb_id : t -> int option
val disambiguation : t -> string
val modified_at : t -> Datetime.t
val created_at  : t -> Datetime.t

val equal : t -> t -> bool

(** {2 Filters} *)

module Filter : sig
  type predicate = [%import: DanceCore.Filter.predicate]
  type t = [%import: DanceCore.Filter.t]

  val accepts : t -> DanceCore.t -> float Lwt.t

  val is : DanceCore.t -> t
  val deviser : PersonCore.Filter.t -> t

  val raw : predicate TextFormula.raw_builder
  val nullary_text_predicates : predicate TextFormula.nullary_predicates
  val unary_text_predicates : predicate TextFormula.unary_predicates

  val from_text_formula : TextFormula.t -> (t, string) Result.t
  val from_string : ?filename:string -> string -> (t, string) Result.t
end

(** {2 Getters and setters} *)

val get : t Slug.t -> t Lwt.t

val make_and_save :
  ?status:Status.t ->
  name:string ->
  kind:Kind.Dance.t ->
  ?deviser:PersonCore.t ->
  two_chords:bool ->
  ?scddb_id:int ->
  ?disambiguation:string ->
  modified_at:Datetime.t ->
  created_at:Datetime.t ->
  unit -> t Lwt.t

val search :
  ?pagination:Pagination.t ->
  ?threshold:float ->
  Filter.t ->
  (int * t Score.t list) Lwt.t
(** [search ?pagination ?threshold filter] returns the list of all the dances
    that match [filter] with a score higher than [threshold] (if any). The first
    element of the pair is the number of dances. The second element of the pair
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
