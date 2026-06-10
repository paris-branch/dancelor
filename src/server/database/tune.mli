open Nes
open Dancelor_common
open Model_new

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

val get : t Entry.id -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> t Entry.id Lwt.t

val update : t Entry.id -> t -> unit Lwt.t

val delete : t Entry.id -> unit Lwt.t

val search : string -> (Tune_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val sql_to_row :
  id: string ->
  name: string ->
  kind: string ->
  composers: Person_name.t list ->
  k: (Tune_row.t -> 'w) ->
  'w
