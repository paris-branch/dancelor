open NesUnix
open Dancelor_common
open Model_new
open Search_new

module Log = (val Logs.src_log @@ Logs.Src.create "server.controller.dance": Logs.LOG)

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)

let to_row (dance : Model.Dance.entry) : Dance_row.t Lwt.t =
  let%lwt devisers = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Dance.devisers' dance in
  let devisers = List.map Person.to_name devisers in
  lwt {
    Dance_row.id = Entry.id dance;
    name = NEString.to_string @@ NEList.hd @@ Model.Dance.names' dance;
    kind = Model.Dance.kind' dance;
    devisers;
    disambiguation = Option.map NEString.to_string @@ Model.Dance.disambiguation' dance;
  }

let to_view (dance : Model.Dance.entry) : Dance_view.t Lwt.t =
  let%lwt devisers = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Dance.devisers' dance in
  let devisers = List.map Person.to_name devisers in
  lwt {
    Dance_view.id = Entry.id dance;
    name = NEString.to_string @@ NEList.hd @@ Model.Dance.names' dance;
    extra_names = List.map NEString.to_string @@ NEList.tl @@ Model.Dance.names' dance;
    kind = Model.Dance.kind' dance;
    devisers;
    scddb_id = Model.Dance.scddb_id' dance;
    disambiguation = Option.map NEString.to_string @@ Model.Dance.disambiguation' dance;
    date = Model.Dance.date' dance;
  }

let get env id =
  match%lwt Database.Dance.get id with
  | None -> Permission.reject_can_get ()
  | Some dance ->
    Permission.assert_can_get_public env dance;%lwt
    lwt dance

let get_row env id =
  to_row =<< get env id

let get_view env id =
  to_view =<< get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt dance = Database.Dance.get id in
      Monadise_lwt.lift_1_1
        Option.iter
        (fun dance ->
          if%lwt Permission.can_get_public env dance then
            Hashtbl.add table id <$> to_row dance
          else
            lwt_unit
        )
        dance
    )
    ids;%lwt
  lwt table

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let create env dance =
  Permission.assert_can_create_public env;%lwt
  Database.Dance.create dance

let update env id dance =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Dance.update id dance

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Dance.delete id

include Search.Build(struct
  type value = Model.Dance.entry
  type filter = (Model.Dance.t, Filter.Dance.t) Formula_entry.public

  let get_all env =
    let all = Database.Dance.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Dance.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Dance.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Dance.one_name') String.Sensible.compare]
end)

let search env slice filter =
  let%lwt result = search env slice filter in
  let%lwt items = Lwt_list.map_s to_row result.items in
  lwt {result with items}

let search'_new env query =
  let%lwt items = Database.Dance.search query in
  let%lwt items = Lwt_list.filter_s (Permission.can_get_public_new env % fst) items in
  lwt {Search_result.total = List.length items; items}

let search_new env slice query =
  let query = {Query.common = {name = Query_string.project query}; specific = {Dance_query.kind = None}} in
  (* FIXME: parsing *)
  let%lwt {total; items} = search'_new env query in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Search_result.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Dance.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Search_new -> search_new env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
