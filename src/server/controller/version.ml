open NesUnix
open Common

module Log = (val Logger.create "controller.version": Logs.LOG)

let get_content env id =
  match%lwt Database.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get env version;%lwt
    lwt @@ Model.Version.content' version

(* TODO: reintroduce metadata *)
(* let%lwt rendering_params = *)
(*   let%lwt pdf_metadata = *)
(*     let%lwt tune = Model.Version.tune' version in *)
(*     let name = Option.value (Model.VersionParameters.display_name version_params) ~default: (Model.Tune.one_name' tune) in *)
(*     let%lwt composers = List.map (NEString.to_string % Model.Person.name') <$> Model.Tune.composers' tune in *)
(*     let subjects = [KindBase.to_pretty_string ~capitalised: true @@ Model.Tune.kind' tune] in *)
(*     lwt @@ *)
(*       RenderingParameters.update_pdf_metadata *)
(*         ~title: (String.replace_empty ~by: (NEString.to_string name)) *)
(*         ~composers: (List.replace_nil ~by: composers) *)
(*         ~subjects: (List.replace_nil ~by: subjects) *)
(*   in *)
(*   lwt @@ RenderingParameters.update ~pdf_metadata rendering_params *)

let get_pdf env id _slug version_params rendering_params =
  match%lwt Model.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get env version;%lwt
    let%lwt fname =
      let%lwt set = ModelToRenderer.version_to_renderer_set' version version_params Model.SetParameters.none in
      let%lwt book_pdf_arg = ModelToRenderer.renderer_set_to_renderer_book_pdf_arg set rendering_params in
      Renderer.make_book_pdf book_pdf_arg
    in
    Madge_server.respond_file ~content_type: "application/pdf" ~fname

let get_svg env id _slug version_params _rendering_params =
  match%lwt Model.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get env version;%lwt
    let%lwt fname =
      let%lwt tune = ModelToRenderer.version_to_renderer_tune' version version_params in
      let stylesheet = "/fonts.css" in
      Renderer.make_tune_svg {tune; stylesheet}
    in
    Madge_server.respond_file ~content_type: "image/svg+xml" ~fname

let preview_svg env version version_params _rendering_params =
  Log.debug (fun m -> m "Model.Version.Svg.preview");
  Permission.assert_can_create env;%lwt
  let%lwt fname =
    let%lwt tune = ModelToRenderer.version_to_renderer_tune version version_params in
    let stylesheet = "/fonts.css" in
    Renderer.make_tune_svg {tune; stylesheet}
  in
  Madge_server.respond_file ~content_type: "image/svg+xml" ~fname

let get_ogg env id _slug version_params _rendering_params =
  Log.debug (fun m -> m "Model.Version.Ogg.get %a" Entry.Id.pp' id);
  match%lwt Database.Version.get id with
  | None -> Permission.reject_can_get ()
  | Some version ->
    Permission.assert_can_get env version;%lwt
    let%lwt fname =
      let%lwt tune = Model.Version.tune' version in
      let kind = Model.Tune.kind' tune in
      let (tempo_unit, tempo_value) = Kind.Base.tempo kind in
      let chords_kind = Kind.Base.to_pretty_string ~capitalised: false kind in
      let%lwt tune = ModelToRenderer.version_to_renderer_tune' version version_params in
      Renderer.make_tune_ogg {tune; tempo_unit; tempo_value; chords_kind}
    in
    Madge_server.respond_file ~content_type: "audio/ogg" ~fname

let preview_ogg env version version_params _rendering_params =
  Log.debug (fun m -> m "Model.Version.Ogg.preview");
  Permission.assert_can_create env;%lwt
  let%lwt fname =
    let%lwt tune = Model.Version.tune version in
    let kind = Model.Tune.kind' tune in
    let (tempo_unit, tempo_value) = Kind.Base.tempo kind in
    let chords_kind = Kind.Base.to_pretty_string ~capitalised: false kind in
    let%lwt tune = ModelToRenderer.version_to_renderer_tune version version_params in
    Renderer.make_tune_ogg {tune; tempo_unit; tempo_value; chords_kind}
  in
  Madge_server.respond_file ~content_type: "audio/ogg" ~fname

let get env id =
  match%lwt Database.Version.get id with
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
    List.filter (Permission.can_get env)
    <$> Database.Version.get_all ()

  let filter_accepts = Filter.Version.accepts

  let tiebreakers =
    Lwt_list.[increasing (NEString.to_string <%> Model.Version.one_name') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Content -> get_content env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Svg -> get_svg env
  | Ogg -> get_ogg env
  | Pdf -> get_pdf env
  | PreviewSvg -> preview_svg env
  | PreviewOgg -> preview_ogg env
