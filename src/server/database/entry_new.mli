open Dancelor_common

type type_ = [
  | `Book
  | `Dance
  | `Person
  | `Set
  | `Source
  | `Tune
  | `User
  | `Version
]

val make : Connection.t -> type_ -> 'any Entry.Id.t Lwt.t
(** Make a globally unique id and register it. *)

val get : Connection.t -> 'any Entry.Id.t -> type_ option Lwt.t
(** Given an id, try to find the corresponding model in the global table. *)
