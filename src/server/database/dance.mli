open Nes
open Dancelor_common
open Model_new
open Search_new

val get_row_for : Dance_id.t list -> (Dance_id.t -> Dance_row.t option) Lwt.t
val get_view : Dance_id.t -> Dance_view.t option Lwt.t
val search : Dance_query.t -> (Dance_row.t * float) list Lwt.t

(** {2 Legacy} *)

type t = Model_builder.Core.Dance.t
type entry = Model_builder.Core.Dance.entry

val get : t Entry.id -> entry option Lwt.t

val create : t -> t Entry.id Lwt.t

val update : t Entry.id -> t -> unit Lwt.t

val delete : t Entry.id -> unit Lwt.t
