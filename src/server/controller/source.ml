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

let to_row (source : Model.Source.entry) : Source_row.t Lwt.t =
  let%lwt editors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Source.editors' source in
  let editors = List.map Person.to_name editors in
  lwt {
    Source_row.id = Entry.id source;
    name = NEString.to_string @@ Model.Source.name' source;
    date = Model.Source.date' source;
    editors;
  }

let to_view (source : Model.Source.entry) : Source_view.t Lwt.t =
  let%lwt editors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Source.editors' source in
  let editors = List.map Person.to_name editors in
  lwt {
    Source_view.id = Entry.id source;
    name = NEString.to_string @@ Model.Source.name' source;
    short_name = Option.map NEString.to_string @@ Model.Source.short_name' source;
    date = Model.Source.date' source;
    editors;
    scddb_id = Model.Source.scddb_id' source;
    description = Model.Source.description' source;
  }

let get env id =
  match%lwt Database.Source.get id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public env source;%lwt
    lwt source

let get_row env id =
  to_row =<< get env id

let get_view env id =
  to_view =<< get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt source = Database.Source.get id in
      Monadise_lwt.lift_1_1
        Option.iter
        (fun source ->
          if%lwt Permission.can_get_public env source then
            Hashtbl.add table id <$> to_row source
          else
            lwt_unit
        )
        source
    )
    ids;%lwt
  lwt table

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

include Search.Build(struct
  type value = Model.Source.entry
  type filter = (Model.Source.t, Filter.Source.t) Formula_entry.public

  let get_all env =
    let all = Database.Source.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Source.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Source.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Source.name') String.Sensible.compare]
end)

let search env slice filter =
  let%lwt result = search env slice filter in
  let%lwt items = Lwt_list.map_s to_row result.items in
  lwt {result with items}

let get_cover env id =
  Permission.assert_can_get_public env =<< get env id;%lwt
  Database.Source.with_cover id @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat (Config.get ()).share "no-cover.webp") in
  Madge_server.respond_file ~fname

let search'_new env query =
  let%lwt items = Database.Source.search query in
  let%lwt items = Lwt_list.filter_s (Permission.can_get_public_new env % fst) items in
  lwt {Search_result.total = List.length items; items}

let search_new env slice query =
  let%lwt {total; items} = search'_new env query in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Search_result.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search_new -> search_new env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Cover -> get_cover env
