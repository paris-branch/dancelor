(** {1 Shared code between controllers}

    Because some things are specific but a lot of things in
    controllers are the same, we centralise all of those here. *)

open Nes
open Dancelor_common
open Search_new

module type Db = sig
  type id
  type row
  type view
  type query

  val get_row : id -> row option Lwt.t
  val get_view : id -> view option Lwt.t
  val get_rows : id list -> (id, row) Database.Utils.tbl Lwt.t
  val search : query -> (row * float) list Lwt.t
end

module Make (Db : Db) = struct
  let get_row env id =
    match%lwt Db.get_row id with
    | None -> Permission.reject_can_get ()
    | Some person ->
      Permission.assert_can_get_public_new env person;%lwt
      lwt person

  let get_view env id =
    match%lwt Db.get_view id with
    | None -> Permission.reject_can_get ()
    | Some person ->
      Permission.assert_can_get_public_new env person;%lwt
      lwt person

  (** Returns a hash table containing as many of the ids as possible. *)
  let get_rows_table env ids =
    let%lwt Tbl tbl = Db.get_rows ids in
    Monadise_lwt.lift_2_1 Hashtbl.filter_map_inplace (fun _id person -> if%lwt Permission.can_get_public_new env person then lwt_some person else lwt_none) tbl;%lwt
    lwt tbl

  let get_rows env ids =
    let%lwt table = get_rows_table env ids in
    lwt @@ List.filter_map (Hashtbl.find_opt table) ids

  let cache : (Environment.cache_key * Db.query, (Db.row * float) Search_result.t Lwt.t) Cache.t =
    Cache.create ~lifetime: 60 ()

  let search' env query =
    Cache.use ~cache ~key: (Environment.cache_key env, query) @@ fun () ->
    let%lwt items = Db.search query in
    let%lwt items = Lwt_list.filter_s (Permission.can_get_public_new env % fst) items in
    lwt {Search_result.total = List.length items; items}

  let search env slice query =
    let%lwt {total; items} = search' env query in
    let items = List.map fst @@ Slice.list ~strict: false slice items in
    lwt {Search_result.total; items}
end
