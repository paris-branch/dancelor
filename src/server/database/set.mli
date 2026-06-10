open Dancelor_common
open Model_new

type t = Model_builder.Core.Set.t
type entry = Model_builder.Core.Set.entry

val get : Set_id.t -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> Entry.Access.Private.t -> Set_id.t Lwt.t

val update : Set_id.t -> t -> Entry.Access.Private.t -> unit Lwt.t

val delete : Set_id.t -> unit Lwt.t

val search : user: User_id.t option -> ?threshold: float -> string -> (Set_row.t * float) list Lwt.t
