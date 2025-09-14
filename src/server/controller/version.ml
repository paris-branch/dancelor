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
  Permission.assert_can_create env;%lwt
  Database.Version.create version

let update env id version =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Version.update id version

let delete env id =
  Permission.assert_can_delete env =<< get env id;%lwt
  Database.Version.delete id

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
  type value = Model.Version.t Entry.t
  type filter = Filter.Version.t

  let get_all env =
    List.filter (Permission.can_get env) (Database.Version.get_all ())

  let filter_accepts = Filter.Version.accepts

  let tiebreakers =
    Lwt_list.[increasing (NEString.to_string <%> Model.Version.one_name') String.Sensible.compare]
end)

let content env id =
  Log.debug (fun m -> m "content %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  Permission.assert_can_get env version;%lwt
  lwt @@ Model.Version.content' version

let build_pdf env id version_params rendering_params =
  Log.debug (fun m -> m "build_pdf %a" Entry.Id.pp' id);
  get env id >>= fun version ->
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
  let%lwt set = ModelToRenderer.version_to_renderer_set' version version_params set_params in
  let%lwt book_pdf_arg = ModelToRenderer.renderer_set_to_renderer_book_pdf_arg set rendering_params pdf_metadata in
  lwt @@ uncurry Job.register_job @@ Renderer.make_book_pdf book_pdf_arg

(** For use in {!Routine}. *)
let render_snippets ?version_params version =
  let%lwt tune = ModelToRenderer.version_to_renderer_tune ?version_params version in
  lwt @@ Renderer.make_tune_snippets tune

let render_svg ?version_params version =
  let%lwt tune = ModelToRenderer.version_to_renderer_tune ?version_params version in
  lwt @@ Renderer.make_tune_svg tune

let build_svg env id version_params _rendering_params =
  Log.debug (fun m -> m "build_svg %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  uncurry Job.register_job <$> render_svg (Entry.value version) ~version_params

let build_svg' env version version_params _rendering_params =
  Log.debug (fun m -> m "build_svg'");
  Permission.assert_can_create env;%lwt
  uncurry Job.register_job <$> render_svg version ~version_params

let render_ogg ?version_params version =
  let%lwt tune = ModelToRenderer.version_to_renderer_tune ?version_params version in
  lwt @@ Renderer.make_tune_ogg tune

let build_ogg env id version_params _rendering_params =
  Log.debug (fun m -> m "build_ogg %a" Entry.Id.pp' id);
  get env id >>= fun version ->
  uncurry Job.register_job <$> render_ogg (Entry.value version) ~version_params

let build_ogg' env version version_params _rendering_params =
  Log.debug (fun m -> m "build_ogg'");
  Permission.assert_can_create env;%lwt
  uncurry Job.register_job <$> render_ogg version ~version_params

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Content -> content env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | BuildPdf -> build_pdf env
  | BuildSvg -> build_svg env
  | BuildSvg' -> build_svg' env
  | BuildOgg -> build_ogg env
  | BuildOgg' -> build_ogg' env
