open Nes
open Dancelor_common
open Model_new
open Search_new

val get_row : Tune_id.t -> Tune_row.t option Lwt.t

val get_rows : Tune_id.t list -> (Tune_id.t, Tune_row.t) Utils.tbl Lwt.t

val get_rows_for_dance : Dance_id.t -> Tune_row.t list Lwt.t

val get_view : Tune_id.t -> Tune_view.t option Lwt.t

val search : Tune_query.t -> (Tune_row.t * float) list Lwt.t

(** {2 Legacy} *)

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

val get : t Entry.id -> entry option Lwt.t

val create : t -> t Entry.id Lwt.t

val update : t Entry.id -> t -> unit Lwt.t

val delete : t Entry.id -> unit Lwt.t

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

val sql_to_view :
  id: string ->
  name: string ->
  extra_names: string list ->
  kind: sql_kind_base ->
  composers: Person_name_with_details.t list ->
  dances: Dance_row.t list ->
  remark: string option ->
  scddb_id: int64 option ->
  date: string option ->
  versions: Tune_view.version_row_without_tune list ->
  k: (Tune_view.t -> 'w) ->
  'w

val sql_to_version_row_without_tune :
  id: string ->
  sources: Source_short_name.t list ->
  disambiguation: string option ->
  arrangers: Person_name.t list ->
  monolithic_bars: int64 option ->
  monolithic_or_default_structure: string option ->
  k: (Tune_view.version_row_without_tune -> 'w) ->
  'w
