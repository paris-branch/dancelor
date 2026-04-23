open Dancelor_common

val make : Model_builder.Core.Any.Type.t -> 'any Entry.Id.t Lwt.t
(** Make a globally unique id and register it. *)

val get : 'any Entry.Id.t -> Model_builder.Core.Any.Type.t option Lwt.t
(** Given an id, try to find the corresponding model in the global table. *)
