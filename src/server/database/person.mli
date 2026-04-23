open Dancelor_common

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

val get : t Entry.id -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> entry Lwt.t

val update : t Entry.id -> t -> entry Lwt.t

val delete : t Entry.id -> unit Lwt.t
