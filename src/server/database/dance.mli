open Nes
open Dancelor_common
open Model_new
open Search_new

val get_row : Dance_id.t -> Dance_row.t option Lwt.t

val get_rows : Dance_id.t list -> (Dance_id.t, Dance_row.t) Utils.tbl Lwt.t

val get_view : Dance_id.t -> Dance_view.t option Lwt.t

val search : Dance_query.t -> (Dance_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val sql_to_row :
  id: string ->
  name: string ->
  kind: string ->
  devisers: Person_name.t list ->
  disambiguation: string option ->
  k: (Dance_row.t -> 'w) ->
  'w

(** {2 Legacy} *)

type t = Model_builder.Core.Dance.t
type entry = Model_builder.Core.Dance.entry

val get : t Entry.id -> entry option Lwt.t

val create : t -> t Entry.id Lwt.t

val update : t Entry.id -> t -> unit Lwt.t

val delete : t Entry.id -> unit Lwt.t
