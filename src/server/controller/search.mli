(** {1 Search controller helpers} *)

open Nes

module type Searchable = sig
  type value
  type filter

  val cache : (float * filter, value list Lwt.t) Cache.t

  val get_all : unit -> value list Lwt.t

  val filter_accepts : filter -> value -> float Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module type S = sig
  type value
  type filter

  val search : Slice.t -> filter -> (int * value list) Lwt.t

  val search' : filter -> value list Lwt.t
  val count : filter -> int Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module Build : functor (M : Searchable) -> S with type value = M.value and type filter = M.filter
