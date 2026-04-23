open Dancelor_common

type t = Model_builder.Core.Source.t
type entry = Model_builder.Core.Source.entry

val get : t Entry.id -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> entry Lwt.t

val update : t Entry.id -> t -> entry Lwt.t

val delete : t Entry.id -> unit Lwt.t

val with_cover : t Entry.id -> (string option -> 'a Lwt.t) -> 'a Lwt.t
(** Given a source id, produce a file containing the cover and pass its path to
    the callback. [None] means that there is no cover for this source. *)
