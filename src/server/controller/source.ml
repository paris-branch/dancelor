open Nes
open Dancelor_common
open Model_new
open Search_new

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)
let to_name (source : Model.Source.entry) : Source_name.t = {
  Source_name.id = Entry.id source;
  name = NEString.to_string @@ Model.Source.name' source;
}
let to_short_name (source : Model.Source.entry) : Source_short_name.t = {
  Source_short_name.id = Entry.id source;
  short_name =
  NEString.to_string (
    match Model.Source.short_name' source with
    | None -> Model.Source.name' source
    | Some name -> name
  );
}

let get env id =
  match%lwt Database.Source.get id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public env source;%lwt
    lwt source

let get_row env id =
  match%lwt Database.Source.get_row id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public_new env source;%lwt
    lwt source

let get_view env id =
  match%lwt Database.Source.get_view id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public_new env source;%lwt
    lwt source

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let%lwt Tbl tbl = Database.Source.get_rows ids in
  Monadise_lwt.lift_2_1 Hashtbl.filter_map_inplace (fun _id source -> if%lwt Permission.can_get_public_new env source then lwt_some source else lwt_none) tbl;%lwt
  lwt tbl

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let create env source =
  Permission.assert_can_create_public env;%lwt
  Database.Source.create source

let update env id source =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Source.update id source

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Source.delete id

let get_cover env id =
  Permission.assert_can_get_public env =<< get env id;%lwt
  Database.Source.with_cover id @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat (Config.get ()).share "no-cover.webp") in
  Madge_server.respond_file ~fname

let search' env query =
  let%lwt items = Database.Source.search query in
  let%lwt items = Lwt_list.filter_s (Permission.can_get_public_new env % fst) items in
  lwt {Search_result.total = List.length items; items}

let search env slice query =
  let%lwt {total; items} = search' env query in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Search_result.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Cover -> get_cover env
