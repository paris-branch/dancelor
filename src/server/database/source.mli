open Nes
open Dancelor_common
open Model_new
open Search_new

type t = Model_builder.Core.Source.t
type entry = Model_builder.Core.Source.entry

val get : Source_id.t -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> Source_id.t Lwt.t

val update : Source_id.t -> t -> unit Lwt.t

val delete : Source_id.t -> unit Lwt.t

val with_cover : Source_id.t -> (string option -> 'a Lwt.t) -> 'a Lwt.t
(** Given a source id, produce a file containing the cover and pass its path to
    the callback. [None] means that there is no cover for this source. *)

val search : Source_query.t -> (Source_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val sql_to_short_name :
  id: string ->
  name: string ->
  short_name: string option ->
  k: (Source_short_name.t -> 'w) ->
  'w
