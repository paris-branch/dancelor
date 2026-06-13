open Nes
open Dancelor_common
open Model_new
open Search_new

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

val get : t Entry.id -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> t Entry.id Lwt.t

val update : t Entry.id -> t -> unit Lwt.t

val delete : t Entry.id -> unit Lwt.t

val search : Tune_query.t -> (Tune_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

type sql_kind_base = [`Jig | `Reel | `Strathspey | `Waltz | `Polka | `Jig_9_8 | `Other]

val sql_to_kind_base : sql_kind_base -> Kind_base.t
val kind_base_to_sql : Kind_base.t -> sql_kind_base

val sql_to_row :
  id: string ->
  name: string ->
  kind: sql_kind_base ->
  composers: Person_name.t list ->
  k: (Tune_row.t -> 'w) ->
  'w
