open Dancelor_common
open Model_new
open Search_new

val get_row : user_id: User_id.t option -> Set_id.t -> Set_row.t option Lwt.t

val get_rows : user_id: User_id.t option -> Set_id.t list -> (Set_id.t, Set_row.t) Utils.tbl Lwt.t

val get_view : user_id: User_id.t option -> Set_id.t -> Set_view.t option Lwt.t

val search : user_id: User_id.t option -> Set_query.t -> (Set_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val get_tunes_for : Connection.t -> Set_id.t Utils.all_or_one_of -> (Set_id.t -> Version_name.t list) Lwt.t

val get_conceptors_for : Connection.t -> Set_id.t Utils.all_or_one_of -> (Set_id.t -> Person_name.t list) Lwt.t

(** {2 Legacy} *)

type t = Model_builder.Core.Set.t
type entry = Model_builder.Core.Set.entry

val get : Set_id.t -> entry option Lwt.t

val create : t -> Entry.Access.Private.t -> Set_id.t Lwt.t

val update : Set_id.t -> t -> Entry.Access.Private.t -> unit Lwt.t

val delete : Set_id.t -> unit Lwt.t
