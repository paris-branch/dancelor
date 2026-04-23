open Dancelor_common

type t = Model_builder.Core.Set.t
type entry = Model_builder.Core.Set.entry

val get : t Entry.id -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> Entry.Access.Private.t -> entry Lwt.t

val update : t Entry.id -> t -> Entry.Access.Private.t -> entry Lwt.t

val delete : t Entry.id -> unit Lwt.t
