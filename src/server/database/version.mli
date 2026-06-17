open Nes
open Dancelor_common
open Model_new
open Search_new

val get_row : Version_id.t -> Version_row.t option Lwt.t

val get_rows : Version_id.t list -> (Version_id.t, Version_row.t) Utils.tbl Lwt.t

val get_view : Version_id.t -> Version_view.t option Lwt.t

val search : Version_query.t -> (Version_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val sql_to_name :
  id: string ->
  name: string ->
  k: (Version_name.t -> 'w) ->
  'w

(** {2 Legacy} *)

type t = Model_builder.Core.Version.t
type entry = Model_builder.Core.Version.entry

val get : Version_id.t -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val get_all_for_tune : Tune_id.t -> entry list Lwt.t

val create : t -> Version_id.t Lwt.t

val update : Version_id.t -> t -> unit Lwt.t

val delete : Version_id.t -> unit Lwt.t
