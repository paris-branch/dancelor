open Nes
open Dancelor_common
open Model_new
open Search_new

val get_row : Person_id.t -> Person_row.t option Lwt.t

val get_rows : Person_id.t list -> (Person_id.t, Person_row.t) Utils.tbl Lwt.t

val get_view : Person_id.t -> Person_view.t option Lwt.t

val get_row_for_user : User_id.t -> Person_row.t option Lwt.t

val search : Person_query.t -> (Person_row.t * float) list Lwt.t

(** {2 Utilities for other models} *)

val sql_to_name :
  id: string ->
  name: string ->
  k: (Person_name.t -> 'w) ->
  'w

val sql_to_name_with_details :
  id: string ->
  name: string ->
  details: string option ->
  k: (Person_name_with_details.t -> 'w) ->
  'w

val sql_to_row :
  id: string ->
  name: string ->
  k: (Person_row.t -> 'w) ->
  'w

(** {2 Legacy} *)

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

val get : Person_id.t -> entry option Lwt.t

val create : t -> Person_id.t Lwt.t

val update : Person_id.t -> t -> unit Lwt.t

val delete : Person_id.t -> unit Lwt.t
