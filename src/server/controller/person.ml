open Nes
open Dancelor_common
open Model_new
open Search_new

(* FIXME: The following conversion functions is temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)
let to_name (person : Model.Person.entry) : Person_name.t = {
  Person_name.id = Entry.id person;
  name = NEString.to_string @@ Model.Person.name' person;
}

let get env id =
  match%lwt Database.Person.get id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public env person;%lwt
    lwt person

let get_row env id =
  match%lwt Database.Person.get_row id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public_new env person;%lwt
    lwt person

let get_view env id =
  match%lwt Database.Person.get_view id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public_new env person;%lwt
    lwt person

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let%lwt Tbl tbl = Database.Person.get_rows ids in
  Monadise_lwt.lift_2_1 Hashtbl.filter_map_inplace (fun _id person -> if%lwt Permission.can_get_public_new env person then lwt_some person else lwt_none) tbl;%lwt
  lwt tbl

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let for_user env id =
  match%lwt Database.Person.get_row_for_user id with
  | None -> lwt_none
  | Some person ->
    Permission.assert_can_get_public_new env person;%lwt
    lwt_some person

let create env person =
  Permission.assert_can_create_public env;%lwt
  Database.Person.create person

let update env id person =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Person.update id person

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Person.delete id

let search' env query =
  let%lwt items = Database.Person.search query in
  let%lwt items = Lwt_list.filter_s (Permission.can_get_public_new env % fst) items in
  lwt {Search_result.total = List.length items; items}

let search env slice query =
  let%lwt {total; items} = search' env query in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Search_result.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | For_user -> for_user env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
