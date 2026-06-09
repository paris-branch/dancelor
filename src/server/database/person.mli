open Nes
open Dancelor_common
open Model_new

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

val get : Person_id.t -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> Person_id.t Lwt.t

val update : Person_id.t -> t -> unit Lwt.t

val delete : Person_id.t -> unit Lwt.t

val search : ?threshold: float -> NEString.t option -> (Person_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val sql_to_name :
  id: string ->
  name: string ->
  k: (Person_name.t -> 'w) ->
  'w
