(** {1 Search controller helpers} *)

open Nes
open Dancelor_common
open Search_new

module type Searchable = sig
  type value
  type filter

  val get_all : Environment.t -> value Lwt_stream.t

  val optimise_filter : filter -> filter
  val filter_is_empty : filter -> bool
  val filter_accepts : filter -> value -> float Lwt.t
  val score_true : float

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module type S = sig
  type value
  type filter

  val search : Environment.t -> Slice.t -> filter -> value Search_result.t Lwt.t

  val search' : Environment.t -> filter -> (value * float) Search_result.t Lwt.t
  (** Variant of {!search} that exposes the whole sequence of values, sorted,
      with their scores, before slicing. *)

  (** Pass through for better composition *)
  val tiebreakers : (value -> value -> int Lwt.t) list
end

module Build : functor (M : Searchable) ->
  S with
  type value = M.value
  and type filter = M.filter
