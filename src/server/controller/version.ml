open NesUnix
open Dancelor_common
open Model_new

module Log = (val Logs.src_log @@ Logs.Src.create "server.controller.version": Logs.LOG)

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)

let to_row (version : Model.Version.entry) : Version_row.t Lwt.t =
  let content_to_content = function
    | Model.Version.Content.No_content -> Version_row.No_content
    | Destructured _ -> Destructured
    | Monolithic {bars; structure; _} -> Monolithic {bars; structure}
  in
  let%lwt tune = Tune.to_row =<< Model.Version.tune' version in
  let%lwt sources = Lwt_list.map_s (Option.get <%> Model.Source.get % Model.Version.source_source) @@ Model.Version.sources' version in
  let sources = List.map Source.to_short_name sources in
  let%lwt arrangers = Lwt_list.map_s (Person.to_name % Option.get <%> Model.Person.get) (Model.Version.arrangers' version) in
  lwt {
    Version_row.id = Entry.id version;
    tune;
    sources;
    disambiguation = Option.map NEString.to_string @@ Model.Version.disambiguation' version;
    arrangers;
    content = content_to_content @@ Model.Version.content' version;
  }

let to_view env (version : Model.Version.entry) : Version_view.t Lwt.t =
  let source_to_source {Model.Version.source; structure; details} =
    let%lwt source = Option.get <$> Model.Source.get source in
    lwt ({
      id = Entry.id source;
      name = NEString.to_string @@ Model.Source.name' source;
      structure;
      details = Option.map NEString.to_string details;
    }: Version_view.source)
  in
  let content_to_content = function
    | Model.Version.Content.No_content -> Version_view.No_content
    | Destructured {default_structure; _} -> Destructured {default_structure}
    | Monolithic {bars; structure; _} -> Monolithic {bars; structure}
  in
  let%lwt tune = Tune.to_view env =<< Model.Version.tune' version in
  let%lwt sources = Lwt_list.map_s source_to_source @@ Model.Version.sources' version in
  let%lwt arrangers = Lwt_list.map_s (Person.to_name % Option.get <%> Model.Person.get) (Model.Version.arrangers' version) in
  lwt {
    Version_view.id = Entry.id version;
    tune;
    sources;
    disambiguation = Option.map NEString.to_string @@ Model.Version.disambiguation' version;
    arrangers;
    content = content_to_content @@ Model.Version.content' version;
    key = Model.Version.key' version;
    remark = Option.map NEString.to_string @@ Model.Version.remark' version;
  }

let get env id =
  match%lwt Database.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get_public env version;%lwt
    lwt version

let get_row env id =
  to_row =<< get env id

let get_view env id =
  to_view env =<< get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt version = Database.Version.get id in
      Monadise_lwt.monadise_1_1
        Option.iter
        (fun version ->
          if%lwt Permission.can_get_public env version then
            Hashtbl.add table id <$> to_row version
          else
            lwt_unit
        )
        version
    )
    ids;%lwt
  lwt table

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let create env version =
  Permission.assert_can_create_public env;%lwt
  Database.Version.create version

let update env id version =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Version.update id version

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Version.delete id

(** Additionnally to the low-level permission system, version content is
    protected by copyright, so we check whether the composer or the publisher of
    the tune agree on this publication *)
let with_copyright_check env version f =
  let%lwt tune = Model.Version.tune' version in
  let%lwt connected = Permission.is_connected env in
  let%lwt composer_agrees =
    let%lwt composers = Lwt_list.map_p (Option.get <%> Model.Person.get % Model.Tune.composer_composer) (Model.Tune.composers' tune) in
    let%lwt arrangers = Lwt_list.map_p (Option.get <%> Model.Person.get) (Model.Version.arrangers' version) in
    lwt (
      composers <> [] (* there must be at least one composer to agree *)
      && List.for_all Model.Person.composed_tunes_are_public' composers
      && List.for_all Model.Person.composed_tunes_are_public' arrangers
    )
  in
  let%lwt publisher_agrees =
    let source_editors_agree source =
      let%lwt editors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Source.editors' source in
      lwt @@ List.exists Model.Person.published_tunes_are_public' editors
    in
    Lwt_list.filter_s source_editors_agree =<< (Lwt_list.map_p (Option.get <%> Model.Source.get % Model.Version.source_source) @@ Model.Version.sources' version)
  in
  (* let's see if we have a reason to agree to showing this version's content;
     if the composer (and arranger) agrees, that's it; otherwise, if there is a
     source and the publisher agrees, that's is; and finally, if we are
     connected, we get a pass (for now) *)
  let reason =
    if composer_agrees then
      Some Endpoints.Version.Composer_agrees
    else
      match publisher_agrees with
      | source :: _ -> Some (Endpoints.Version.Publisher_agrees source)
      | [] ->
        if connected then
          Some Endpoints.Version.Connected
        else None
  in
  match reason with
  | None -> lwt Endpoints.Version.Protected
  | Some reason ->
    let%lwt payload = f () in
    lwt (Endpoints.Version.Granted {payload; reason})

let can_get_and_copyright_ok env version =
  Lwt.l2
    (&&)
    (Permission.can_get_public env version)
    (((<>) Endpoints.Version.Protected) <$> with_copyright_check env version (const lwt_unit))

let get_view_for_tune env id =
  let all = Database.Version.get_all_for_tune id in
  let stream = (Lwt_stream.filter_s (can_get_and_copyright_ok env) % Lwt_stream.of_list) <$> all in
  let stream = Lwt_stream.flip_lwt stream in
  (* FIXME: some logic to choose a “good” version? *)
  match%lwt Lwt_stream.get stream with
  | Some version -> (fun v -> Endpoints.Version.Version_view_fallback.Found v) <$> to_view env version
  | None -> (fun t -> Endpoints.Version.Version_view_fallback.Fallback t) <$> Tune.get_view env id

let rec search_and_extract acc s regexp =
  let rem = Str.replace_first regexp "" s in
  try
    let gp = Str.matched_group 1 s in
    let gp_words =
      String.split_on_char ',' gp
      |> List.map (String.remove_char '"')
      |> List.map (String.remove_char '\'')
      |> List.filter (fun s -> s <> "")
    in
    let rem, l = search_and_extract acc rem regexp in
    rem, gp_words @ l
  with
    | Not_found | Invalid_argument _ -> rem, acc

let score_list_vs_word words needle =
  List.map (String.inclusion_proximity ~char_equal: Char.Sensible.equal ~needle) words
  |> List.fold_left max 0.

let score_list_vs_list words needles =
  if needles = [] then 1.
  else
    begin
      List.map (score_list_vs_word words) needles
      |> List.fold_left max 0.
    end

include Search.Build(struct
  type value = Model.Version.entry
  type filter = (Model.Version.t, Filter.Version.t) Formula_entry.public

  let get_all env =
    let all = Database.Version.get_all () in
    let stream = (Lwt_stream.filter_s (can_get_and_copyright_ok env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Version.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Version.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (NEString.to_string <%> Model.Version.one_name') String.Sensible.compare]
end)

let content env id =
  Log.debug (fun m -> m "content %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  with_copyright_check env version @@ fun () ->
  lwt @@ Model.Version.content' version

let build_pdf env id version_params rendering_params =
  Log.debug (fun m -> m "build_pdf %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  with_copyright_check env version @@ fun () ->
  (* never show the headers for a simple version *)
  let rendering_params =
    Rendering_parameters.update
      ~show_headers: (const (some false))
      rendering_params
  in
  let%lwt pdf_metadata =
    let%lwt tune = Model.Version.tune' version in
    let title =
      NEString.to_string @@
        Option.value
          (Model.Version_parameters.display_name version_params)
          ~default: (Model.Tune.one_name' tune)
    in
    let%lwt authors =
      Model_to_renderer.format_persons_list
      <$> (
          Lwt_list.map_p
            (Option.get <%> Model.Person.get % Model.Tune.composer_composer)
            (Model.Tune.composers' tune)
        )
    in
    let subjects = [Kind.Base.to_long_string ~capitalised: true @@ Model.Tune.kind' tune] in
    lwt Renderer.{title; authors; subjects}
  in
  let set_params = Model.Set_parameters.make ?display_name: (Model.Version_parameters.display_name version_params) () in
  let version_params = Model.Version_parameters.set_display_name (NEString.of_string_exn " ") version_params in
  let%lwt set = Model_to_renderer.versions_to_renderer_set' (NEList.singleton (Entry.id version, version_params)) set_params in
  let%lwt book_pdf_arg = Model_to_renderer.renderer_set_to_renderer_book_pdf_arg set rendering_params pdf_metadata in
  uncurry Job.register_job <$> Renderer.make_book_pdf book_pdf_arg

(** For use in {!Routine}. *)
let render_snippets ?version_params version =
  let%lwt tune = Model_to_renderer.version_to_renderer_tune ?version_params version in
  Renderer.make_tune_snippets tune

let register_snippets_job ?version_params version =
  let%lwt tune = Model_to_renderer.version_to_renderer_tune ?version_params version in
  let%lwt svg_job = Renderer.make_tune_svg tune in
  let%lwt ogg_job = Renderer.make_tune_ogg tune in
  lwt @@
    match (uncurry Job.register_job svg_job, uncurry Job.register_job ogg_job) with
    | Already_succeeded svg_job_id, Already_succeeded ogg_job_id -> Endpoints.Job.Already_succeeded Endpoints.Version.Snippet_ids.{svg_job_id; ogg_job_id}
    | Registered svg_job_id, Already_succeeded ogg_job_id -> Registered {svg_job_id; ogg_job_id}
    | Already_succeeded svg_job_id, Registered ogg_job_id -> Registered {svg_job_id; ogg_job_id}
    | Registered svg_job_id, Registered ogg_job_id -> Registered {svg_job_id; ogg_job_id}

let build_snippets env id version_params _rendering_params =
  Log.debug (fun m -> m "build_snippets %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  with_copyright_check env version @@ fun () ->
  register_snippets_job ~version_params (Entry.value version)

let build_snippets' env version version_params _rendering_params =
  Log.debug (fun m -> m "build_snippets'");
  Permission.assert_can_create_public env;%lwt
  register_snippets_job ~version_params version

let search env slice filter =
  let%lwt result = search env slice filter in
  let%lwt items = Lwt_list.map_s to_row result.items in
  lwt {result with items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Get_view_for_tune -> get_view_for_tune env
  | Content -> content env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Build_pdf -> build_pdf env
  | Build_snippets -> build_snippets env
  | Build_snippets' -> build_snippets' env
