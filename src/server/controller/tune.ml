open Nes
open Dancelor_common
open Model_new

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)

let to_row (tune : Model.Tune.entry) : Tune_row.t Lwt.t =
  let%lwt composers = Lwt_list.map_s (Option.get <%> Model.Person.get % Model.Tune.composer_composer) @@ Model.Tune.composers' tune in
  let composers = List.map Person.to_name composers in
  lwt {
    Tune_row.id = Entry.id tune;
    name = NEString.to_string @@ NEList.hd @@ Model.Tune.names' tune;
    kind = Model.Tune.kind' tune;
    composers;
  }

let version_to_row_without_tune (version : Model.Version.entry) : Tune_view.version_row_without_tune Lwt.t =
  let content_to_content = function
    | Model.Version.Content.No_content -> Version_row.No_content
    | Destructured _ -> Destructured
    | Monolithic {bars; structure; _} -> Monolithic {bars; structure}
  in
  let%lwt sources = Lwt_list.map_s (Option.get <%> Model.Source.get % Model.Version.source_source) @@ Model.Version.sources' version in
  let sources = List.map Source.to_short_name sources in
  let%lwt arrangers = Lwt_list.map_s (Person.to_name % Option.get <%> Model.Person.get) (Model.Version.arrangers' version) in
  lwt ({
    id = Entry.id version;
    sources;
    disambiguation = Option.map NEString.to_string @@ Model.Version.disambiguation' version;
    arrangers;
    content = content_to_content @@ Model.Version.content' version;
  }: Tune_view.version_row_without_tune)

let to_view env (tune : Model.Tune.entry) : Tune_view.t Lwt.t =
  let%lwt composers =
    Lwt_list.map_s (fun composer ->
      let id = Model.Tune.composer_composer composer in
      let%lwt person = Option.get <$> Model.Person.get id in
      lwt {
        Person_name_with_details.id;
        name = NEString.to_string @@ Model.Person.name' person;
        details = Option.map NEString.to_string @@ Model.Tune.composer_details composer;
      }
    ) @@
      Model.Tune.composers' tune
  in
  let%lwt dances = Lwt_list.map_s (Option.get <%> Model.Dance.get) @@ Model.Tune.dances' tune in
  let%lwt dances = Lwt_list.map_s Dance.to_row dances in
  let%lwt versions = Database.Version.get_all_for_tune (Entry.id tune) in
  let%lwt versions = Lwt_list.filter_s (Permission.can_get_public env) versions in
  let%lwt versions = Lwt_list.map_s version_to_row_without_tune versions in
  lwt {
    Tune_view.id = Entry.id tune;
    name = NEString.to_string @@ NEList.hd @@ Model.Tune.names' tune;
    extra_names = List.map NEString.to_string @@ NEList.tl @@ Model.Tune.names' tune;
    kind = Model.Tune.kind' tune;
    composers;
    dances;
    remark = Option.map NEString.to_string @@ Model.Tune.remark' tune;
    scddb_id = Model.Tune.scddb_id' tune;
    date = Model.Tune.date' tune;
    versions;
  }

let get env id =
  match%lwt Database.Tune.get id with
  | None -> Permission.reject_can_get ()
  | Some tune ->
    Permission.assert_can_get_public env tune;%lwt
    lwt tune

let get_row env id =
  to_row =<< get env id

let get_view env id =
  to_view env =<< get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt tune = Database.Tune.get id in
      Monadise_lwt.lift_1_1
        Option.iter
        (fun tune ->
          if%lwt Permission.can_get_public env tune then
            Hashtbl.add table id <$> to_row tune
          else
            lwt_unit
        )
        tune
    )
    ids;%lwt
  lwt table

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let create env tune =
  Permission.assert_can_create_public env;%lwt
  Database.Tune.create tune

let update env id tune =
  Permission.assert_can_update_public env =<< get env id;%lwt
  ignore <$> Database.Tune.update id tune

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Tune.delete id

include Search.Build(struct
  type value = Model.Tune.entry
  type filter = (Model.Tune.t, Filter.Tune.t) Formula_entry.public

  let get_all env =
    let all = Database.Tune.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Tune.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Tune.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Tune.one_name') String.Sensible.compare;
    increasing (lwt % NEString.to_string % Model.Tune.one_name') String.compare_lengths;
    ]
end)

let search env slice filter =
  let%lwt result = search env slice filter in
  let%lwt items = Lwt_list.map_s to_row result.items in
  lwt {result with items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
