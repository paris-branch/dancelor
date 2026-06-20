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
