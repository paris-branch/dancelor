open Nes
open Common

let get env id =
  match%lwt Database.Set.get id with
  | None -> Permission.reject_can_get ()
  | Some set ->
    Permission.assert_can_get env set;%lwt
    lwt set

let create env set =
  Permission.assert_can_create env;%lwt
  Database.Set.create set

let update env id set =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Set.update id set

let delete env id =
  Permission.assert_can_delete env =<< get env id;%lwt
  Database.Set.delete id

include Search.Build(struct
  type value = Model.Set.t Entry.t
  type filter = Filter.Set.t

  let get_all env =
    List.filter (Permission.can_get env)
    <$> Database.Set.get_all ()

  let filter_accepts = Filter.Set.accepts

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
        Option.value (Model.SetParameters.display_name set_params) ~default: (Model.Set.name' set)
    in
    let%lwt authors = ModelToRenderer.format_persons_list <$> Model.Set.conceptors' set in
    let subjects =
      match KindDance.to_simple @@ Model.Set.kind' set with
      | None -> ["Medley"]
      | Some (n, bars, base) -> [KindBase.to_pretty_string ~capitalised: true base; spf "%dx%d" n bars]
    in
    lwt Renderer.{title; authors; subjects; creator = "FIXME"}
  in
  let%lwt set = ModelToRenderer.set_to_renderer_set' set set_params in
  let%lwt book_pdf_arg =
    ModelToRenderer.renderer_set_to_renderer_book_pdf_arg
      set
      rendering_params
      pdf_metadata
  in
  Renderer.make_book_pdf book_pdf_arg

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | BuildPdf -> build_pdf env
