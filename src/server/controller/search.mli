(** {1 Search controller helpers} *)

open Nes

module type Searchable = sig
  type value
  type filter

  val get_all : Environment.t -> value list

  val filter_accepts : filter -> value -> float Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module type S = sig
  type value
  type filter

  val search : Environment.t -> Slice.t -> filter -> (int * value list) Lwt.t

  (** Pass through for better composition *)
  val get_all : Environment.t -> value list
  val tiebreakers : (value -> value -> int Lwt.t) list
end

module Build : functor (M : Searchable) ->
  S with
  type value = M.value
  and type filter = M.filter
