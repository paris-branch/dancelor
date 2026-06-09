open Nes
open Dancelor_common
open Model_new

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)

let version_to_name (version : Model.Version.entry) : Version_name.t Lwt.t =
  let%lwt tune = Model.Version.tune' version in
  lwt {
    Version_name.id = Entry.id version;
    name = NEString.to_string @@ NEList.hd @@ Model.Tune.names' tune;
  }

let to_row env (set : Model.Set.entry) : Set_row.t Lwt.t =
  let user = Environment.user env in
  let%lwt conceptors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Set.conceptors' set in
  let conceptors = List.map Person.to_name conceptors in
  let%lwt tunes = Lwt_list.map_s (Option.get <%> Model.Version.get % fst) @@ Model.Set.contents' set in
  let%lwt tunes = Lwt_list.map_s version_to_name tunes in
  lwt {
    Set_row.id = Entry.id set;
    name = NEString.to_string @@ Model.Set.name' set;
    kind = Model.Set.kind' set;
    conceptors;
    tunes;
    permission = Option.get @@ Permission.With_reason.can_get_private user set;
  }

let to_view env (set : Model.Set.entry) : Set_view.t Lwt.t =
  let user = Environment.user env in
  let%lwt conceptors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Set.conceptors' set in
  let conceptors = List.map Person.to_name conceptors in
  let%lwt contents =
    Lwt_list.map_s (fun (version, params) ->
      let%lwt version = Option.get <$> Model.Version.get version in
      let%lwt version = Version.to_row version in
      lwt (version, params)
    ) @@
      Model.Set.contents' set
  in
  lwt {
    Set_view.id = Entry.id set;
    name = NEString.to_string @@ Model.Set.name' set;
    kind = Model.Set.kind' set;
    conceptors;
    contents;
    order = Model.Set.order' set;
    remark = Option.map NEString.to_string @@ Model.Set.remark' set;
    permission = Option.get @@ Permission.With_reason.can_get_private user set;
  }

let get env id =
  match%lwt Database.Set.get id with
  | None -> Permission.reject_can_get ()
  | Some set ->
    Permission.assert_can_get_private env set;%lwt
    lwt set

let get_row env id =
  to_row env =<< get env id

let get_view env id =
  to_view env =<< get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt set = Database.Set.get id in
      Monadise_lwt.lift_1_1
        Option.iter
        (fun set ->
          if%lwt Permission.can_get_private env set then
            Hashtbl.add table id <$> to_row env set
          else
            lwt_unit
        )
        set
    )
    ids;%lwt
  lwt table

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let create env set access =
  Permission.assert_can_create_private env;%lwt
  Database.Set.create set access

let update env id set access =
  let%lwt entry = get env id in
  Permission.assert_can_update_private env entry;%lwt
  Database.Set.update id set access

let delete env id =
  Permission.assert_can_delete_private env =<< get env id;%lwt
  Database.Set.delete id

include Search.Build(struct
  type value = Model.Set.entry
  type filter = (Model.Set.t, Filter.Set.t) Formula_entry.private_

  let get_all env =
    let all = Database.Set.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_private env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_private Filter.Set.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_private Model.User.get Filter.Set.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Set.name') String.Sensible.compare;
    increasing (lwt % NEString.to_string % Model.Set.name') String.compare_lengths;
    ]
end)

let build_pdf env id set_params rendering_params =
  get env id >>= fun set ->
  let%lwt pdf_metadata =
    let title =
      NEString.to_string @@
        Option.value (Model.Set_parameters.display_name set_params) ~default: (Model.Set.name' set)
    in
    let%lwt authors = Model_to_renderer.format_persons_list <$> Lwt_list.map_p (Option.get <%> Model.Person.get) (Model.Set.conceptors' set) in
    let subjects =
      match Kind.Dance.to_simple @@ Model.Set.kind' set with
      | None -> ["Medley"]
      | Some (n, bars, base) -> [Kind.Base.to_long_string ~capitalised: true base; spf "%dx%d" n bars]
    in
    lwt Renderer.{title; authors; subjects}
  in
  let%lwt set = Model_to_renderer.set_to_renderer_set' (Entry.id set) set_params in
  let%lwt book_pdf_arg = Model_to_renderer.renderer_set_to_renderer_book_pdf_arg set rendering_params pdf_metadata in
  uncurry Job.register_job <$> Renderer.make_book_pdf book_pdf_arg

let search env slice filter =
  let%lwt result = search env slice filter in
  let%lwt items = Lwt_list.map_s (to_row env) result.items in
  lwt {result with items}

let search'_new env filter =
  let user = Environment.user env in
  let%lwt items = Database.Set.search ~user: (Option.map Entry.id user) filter in
  lwt {total = List.length items; items}

let search_new env slice filter =
  let%lwt {total; items} = search'_new env filter in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Get_rows -> get_rows env
  | Search -> search env
  | Search_new -> search_new env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Build_pdf -> build_pdf env
