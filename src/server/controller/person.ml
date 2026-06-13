open Nes
open Dancelor_common
open Model_new
open Search_new

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)

let to_name (person : Model.Person.entry) : Person_name.t = {
  Person_name.id = Entry.id person;
  name = NEString.to_string @@ Model.Person.name' person;
}

let to_row (person : Model.Person.entry) : Person_row.t = {
  Person_row.id = Entry.id person;
  name = NEString.to_string @@ Model.Person.name' person;
}

let to_view (person : Model.Person.entry) : Person_view.t = {
  Person_view.id = Entry.id person;
  name = NEString.to_string @@ Model.Person.name' person;
  scddb_id = Model.Person.scddb_id' person;
  composed_tunes_are_public = Model.Person.composed_tunes_are_public' person;
  published_tunes_are_public = Model.Person.published_tunes_are_public' person;
}

let get env id =
  match%lwt Database.Person.get id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public env person;%lwt
    lwt person

let get_row env id =
  to_row <$> get env id

let get_view env id =
  to_view <$> get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt person = Database.Person.get id in
      Monadise_lwt.lift_1_1
        Option.iter
        (fun person ->
          if%lwt Permission.can_get_public env person then
            lwt @@ Hashtbl.add table id @@ to_row person
          else
            lwt_unit
        )
        person
    )
    ids;%lwt
  lwt table

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let for_user env id =
  match%lwt Database.User.get_person id with
  | None -> lwt_none
  | Some person_id ->
    match%lwt Database.Person.get person_id with
    | None -> lwt_none
    | Some person ->
      if%lwt Permission.can_get_public env person then
        lwt_some person
      else
        lwt_none

let for_user_row env id =
  Option.map to_row <$> for_user env id

let create env person =
  Permission.assert_can_create_public env;%lwt
  Database.Person.create person

let update env id person =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Person.update id person

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Person.delete id

include Search.Build(struct
  type value = Model.Person.entry
  type filter = (Model.Person.t, Filter.Person.t) Formula_entry.public

  let get_all env =
    let all = Database.Person.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Person.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Person.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Person.name') String.Sensible.compare]
end)

let search env slice filter =
  let%lwt result = search env slice filter in
  lwt {result with items = List.map to_row result.items}

let search'_new env query =
  let%lwt items = Database.Person.search query in
  let%lwt items = Lwt_list.filter_s (Permission.can_get_public_new env % fst) items in
  lwt {Search_result.total = List.length items; items}

let search_new env slice query =
  let query = {Query.common = {name = Query_string.project query}; specific = ()} in
  (* FIXME: parsing *)
  let%lwt {total; items} = search'_new env query in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Search_result.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Search_new -> search_new env
  | For_user_row -> for_user_row env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
