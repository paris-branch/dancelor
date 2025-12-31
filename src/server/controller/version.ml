open NesUnix
open Common

module Log = (val Logger.create "controller.version": Logs.LOG)

let get env id =
  match Database.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get env version;%lwt
    lwt version

let create env version =
  Permission.assert_can_create env @@ fun user ->
  Database.Version.create ~owner: (Entry.id user) version

let update env id version =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Version.update id version

let delete env id =
  Permission.assert_can_delete env =<< get env id;%lwt
  Database.Version.delete id

(** Additionnally to the low-level permission system, version content is
    protected by copyright, so we check whether the composer or the publisher of
    the tune agree on this publication *)
let with_copyright_check env version f =
  let%lwt tune = Model.Version.tune' version in
  let connected = Permission.is_connected env in
  let%lwt composer_agrees =
    let%lwt composers = Model.Tune.composers' tune in
    let%lwt arrangers = Model.Version.arrangers' version in
    lwt (
      composers <> [] (* there must be at least one composer to agree *)
      && List.for_all Model.Person.composed_tunes_are_public' composers
      && List.for_all Model.Person.composed_tunes_are_public' arrangers
    )
  in
  let%lwt publisher_agrees =
    let source_editors_agree source =
      let%lwt editors = Model.Source.editors' source in
      lwt @@ List.exists Model.Person.published_tunes_are_public' editors
    in
    Lwt_list.filter_s source_editors_agree =<< (List.map Model.Version.source_source <$> Model.Version.sources' version)
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
  type filter = Filter.Version.t

  let get_all env =
    let can_get_and_copyright_ok version =
      (&&) (Permission.can_get env version)
      <$> (
          ((<>) Endpoints.Version.Protected)
          <$> with_copyright_check env version (const lwt_unit)
        )
    in
    Lwt_stream.filter_s can_get_and_copyright_ok @@ Lwt_stream.of_seq @@ Database.Version.get_all ()

  let optimise_filter = Filter.Version.optimise
  let filter_is_empty = (=) Formula.False
  let filter_is_full = (=) Formula.True
  let filter_accepts = Filter.Version.accepts
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
    RenderingParameters.update
      ~show_headers: (const (some false))
      rendering_params
  in
  let%lwt pdf_metadata =
    let%lwt tune = Model.Version.tune' version in
    let title =
      NEString.to_string @@
        Option.value
          (Model.VersionParameters.display_name version_params)
          ~default: (Model.Tune.one_name' tune)
    in
    let%lwt authors = ModelToRenderer.format_persons_list <$> Model.Tune.composers' tune in
    let subjects = [KindBase.to_pretty_string ~capitalised: true @@ Model.Tune.kind' tune] in
    lwt Renderer.{title; authors; subjects}
  in
  let set_params = Model.SetParameters.make ?display_name: (Model.VersionParameters.display_name version_params) () in
  let version_params = Model.VersionParameters.set_display_name (NEString.of_string_exn " ") version_params in
  let%lwt set = ModelToRenderer.versions_to_renderer_set' (NEList.singleton (version, version_params)) set_params in
  let%lwt book_pdf_arg = ModelToRenderer.renderer_set_to_renderer_book_pdf_arg set rendering_params pdf_metadata in
  lwt @@ uncurry Job.register_job @@ Renderer.make_book_pdf book_pdf_arg

(** For use in {!Routine}. *)
let render_snippets ?version_params version =
  let%lwt tune = ModelToRenderer.version_to_renderer_tune ?version_params version in
  lwt @@ Renderer.make_tune_snippets tune

let register_snippets_job ?version_params version =
  let%lwt tune = ModelToRenderer.version_to_renderer_tune ?version_params version in
  let svg_job = Renderer.make_tune_svg tune in
  let ogg_job = Renderer.make_tune_ogg tune in
  lwt @@
    match (uncurry Job.register_job svg_job, uncurry Job.register_job ogg_job) with
    | AlreadySucceeded svg_job_id, AlreadySucceeded ogg_job_id -> Endpoints.Job.AlreadySucceeded Endpoints.Version.Snippet_ids.{svg_job_id; ogg_job_id}
    | Registered svg_job_id, AlreadySucceeded ogg_job_id -> Registered {svg_job_id; ogg_job_id}
    | AlreadySucceeded svg_job_id, Registered ogg_job_id -> Registered {svg_job_id; ogg_job_id}
    | Registered svg_job_id, Registered ogg_job_id -> Registered {svg_job_id; ogg_job_id}

let build_snippets env id version_params _rendering_params =
  Log.debug (fun m -> m "build_snippets %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  with_copyright_check env version @@ fun () ->
  register_snippets_job ~version_params (Entry.value version)

let build_snippets' env version version_params _rendering_params =
  Log.debug (fun m -> m "build_snippets'");
  Permission.assert_can_create env @@ fun _ ->
  register_snippets_job ~version_params version

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Content -> content env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | BuildPdf -> build_pdf env
  | BuildSnippets -> build_snippets env
  | BuildSnippets' -> build_snippets' env
