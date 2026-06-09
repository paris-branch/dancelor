open Dancelor_common
open Model_new

type t = Model_builder.Core.Book.t
type entry = Model_builder.Core.Book.entry

val get : Book_id.t -> entry option Lwt.t

(* FIXME: we should really rather provide a fold function, or directly an Lwt_stream or something *)
val get_all : unit -> entry list Lwt.t

val create : t -> Entry.Access.Private.t -> Book_id.t Lwt.t

val update : Book_id.t -> t -> Entry.Access.Private.t -> unit Lwt.t

val delete : Book_id.t -> unit Lwt.t

val search : user: User_id.t option -> string -> (Book_row.t * float) list Lwt.t
