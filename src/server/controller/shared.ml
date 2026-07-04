(** {1 Shared code between controllers}

    Because some things are specific but a lot of things in
    controllers are the same, we centralise all of those here. *)

open Nes
open Dancelor_common
open Model_new
open Search_new

module type Db_private = sig
  type id
  type row
  type view
  type query

  val get_row_for : user_id: User_id.t option -> id list -> (id -> row option) Lwt.t
  val get_view : user_id: User_id.t option -> id -> view option Lwt.t
  val search : user_id: User_id.t option -> query -> (row * float) list Lwt.t
end

module Make_private (Db : Db_private) = struct
  let get_row_for env ids =
    let user = Environment.user env in
    Db.get_row_for ~user_id: (Option.map Entry.id user) ids

  let get_row env id =
    match%lwt (fun f -> f id) <$> get_row_for env [id] with
    | None -> Permission.reject_can_get ()
    | Some row -> lwt row

  let get_rows env ids =
    let%lwt row_for = get_row_for env ids in
    lwt @@ List.filter_map row_for ids

  let get_view env id =
    let user = Environment.user env in
    match%lwt Db.get_view ~user_id: (Option.map Entry.id user) id with
    | None -> Permission.reject_can_get ()
    | Some person -> lwt person

  let cache : (Environment.cache_key * Db.query, (Db.row * float) Search_result.t Lwt.t) Cache.t =
    Cache.create ~lifetime: 60 ()

  let search' env query =
    Cache.use ~cache ~key: (Environment.cache_key env, query) @@ fun () ->
    let user = Environment.user env in
    let%lwt items = Db.search ~user_id: (Option.map Entry.id user) query in
    lwt {Search_result.total = List.length items; items}

  let search env slice query =
    let%lwt {total; items} = search' env query in
    let items = List.map fst @@ Slice.list ~strict: false slice items in
    lwt {Search_result.total; items}
end

module type Db_public = sig
  type id
  type row
  type view
  type query

  val get_row_for : id list -> (id -> row option) Lwt.t
  val get_view : id -> view option Lwt.t
  val search : query -> (row * float) list Lwt.t
end

module Make_public (Db : Db_public) = Make_private(struct
  include Db
  let get_row_for ~user_id: _ = get_row_for
  let get_view ~user_id: _ = get_view
  let search ~user_id: _ = search
end)
