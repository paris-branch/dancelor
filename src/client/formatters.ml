open Nes
open Js_of_ocaml
open Dancelor_client_elements

module M = Dancelor_client_model
module Router = Dancelor_common.Router

module Html = Dom_html

let js = Js.string

(* Generic node creator *)
(* FIXME: all these should probably go in their own helper module. *)

let make_node ?(classes=[]) create page =
  let node = create (Page.document page) in
  List.iter (fun class_ -> node##.classList##add (js class_)) classes;
  node

let make_node_with_content ?classes ~content create page =
  let node = make_node ?classes create page in
  Lwt.on_success content @@ JsHelpers.add_children node;
  node

(* Text nodes have a special handling *)

let text_lwt text page =
  let text_node = (Page.document page)##createTextNode (js "") in
  Lwt.on_success text (fun text -> text_node##.data := js text);
  text_node

let text text page =
  text_lwt (Lwt.return text) page

let space = text " "

(* Other nodes *)

let span ?classes content page =
  make_node_with_content ?classes ~content Html.createSpan page

let span_static ?classes content page =
  span ?classes (Lwt.return content) page

let link_lwt ?classes ~href content page =
  let link = make_node_with_content ?classes ~content Html.createA page in
  Lwt.on_success href (fun href -> link##.href := js href);
  link

let link ?classes ~href content page =
  link_lwt ?classes ~href (Lwt.return content) page

(* Formatters *)

module Kind = struct

  let full_string version tune =
    let open Lwt in
    let%lwt base = M.Tune.kind tune >|= M.Kind.base_to_char in
    let%lwt bars = M.Version.bars version in
    Lwt.return (spf "%i %c" bars base)

  let full_string_lwt version tune =
    let%lwt version = version in
    let%lwt tune = tune in
    full_string version tune

end

module Credit = struct

  let line credit page =
    match credit with
    | None -> Lwt.return_nil
    | Some credit ->
      let href =
        let%lwt slug = M.Credit.slug credit in
        Lwt.return (Router.path_of_controller (Router.Credit slug) |> snd)
      in
      Lwt.return [(link ~href [text_lwt (M.Credit.line credit) page] page :> Dom.node Js.t)]

end

module Tune = struct

  let description tune page =
    let%lwt kind = M.Tune.kind tune in
    let kind = M.Kind.base_to_pretty_string kind in
    let%lwt author = M.Tune.author tune in
    match author with
    | None ->
      Lwt.return [
        (text (String.capitalize_ascii kind) page :> Dom.node Js.t)
      ]
    | Some author when M.Credit.is_trad author ->
      Lwt.return [
        (text ("Traditional " ^ kind) page :> Dom.node Js.t)
      ]
    | Some author ->
      let%lwt line_block = Credit.line (Some author) page in
      Lwt.return (
        (text (String.capitalize_ascii kind ^ " by ") page :> Dom.node Js.t)
        :: line_block
      )

  let aka tune =
    match%lwt M.Tune.alternative_names tune with
    | [] -> Lwt.return ""
    | names -> spf "Also known as %s" (String.concat ", " names) |> Lwt.return

  let aka_lwt tune =
    Lwt.bind tune aka

  let recommended tune =
    match%lwt M.Tune.dances tune with
    | [] -> Lwt.return ""
    | dances ->
      let%lwt dance_names = Lwt_list.map_p M.Dance.name dances in
      spf "Recommended for %s" (String.concat ", " dance_names)
      |> Lwt.return

  let recommended_lwt tune =
    Lwt.bind tune recommended
end

module Version = struct

  let description version page =
    let%lwt bars = M.Version.bars version in
    let%lwt structure = M.Version.structure version in
    let%lwt key = M.Version.key version in
    let shape = spf "%d-bar %s version in %s" bars structure (M.Music.key_to_pretty_string key) in
    let%lwt arranger_block =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return []
      | Some arranger ->
        let%lwt line_block = Credit.line (Some arranger) page in
        Lwt.return (
          (text " arranged by " page :> Dom.node Js.t)
          :: line_block
        )
    in
    let%lwt disambiguation_block =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return []
      | disambiguation ->
        Lwt.return [(text (spf " (%s)" disambiguation) page :> Dom.node Js.t)]
    in
    Lwt.return (
      [(text shape page :> Dom.node Js.t)]
      @ arranger_block
      @ disambiguation_block
    )

  let name version page =
    let href =
      let%lwt slug = M.Version.slug version in
      Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
    in
    let name =
      let%lwt tune = M.Version.tune version in
      M.Tune.name tune
    in
    Lwt.return [(link ~href [text_lwt name page] page :> Dom.node Js.t)]

  let name_and_disambiguation version page =
    let%lwt name_block = name version page in
    let%lwt disambiguation_block =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return_nil
      | disambiguation -> Lwt.return [
          (span_static ~classes:["dim"] [text (spf " (%s)" disambiguation) page] page :> Dom.node Js.t)
        ]
    in
    Lwt.return (name_block @ disambiguation_block)

  let author_and_arranger ?(short=true) version page =
    let%lwt author_block =
      let%lwt tune = M.Version.tune version in
      match%lwt M.Tune.author tune with
      | None -> Lwt.return []
      | Some author -> Credit.line (Some author) page
    in
    let has_author =
      let%lwt tune = M.Version.tune version in
      match%lwt M.Tune.author tune with
      | None -> Lwt.return_false
      | Some _ -> Lwt.return_true
    in
    let%lwt arranger_block =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return []
      | Some arranger ->
        let%lwt comma = if%lwt has_author then Lwt.return ", " else Lwt.return "" in
        let arr = if short then "arr." else "arranged by" in
        let%lwt arranger_block = Credit.line (Some arranger) page in
        Lwt.return [
          (span_static ~classes:["dim"] (
              (text (spf "%s%s " comma arr) page :> Dom.node Js.t)
              :: arranger_block
            ) page :> Dom.node Js.t)
        ]
    in
    Lwt.return (
      author_block
      @ arranger_block
    )

end

module Set = struct

  let works set =
    match%lwt M.Set.dances set with
    | [] -> Lwt.return ""
    | dances ->
      let%lwt dance_names = Lwt_list.map_p M.Dance.name dances in
      spf "Works for %s" (String.concat ", " dance_names)
      |> Lwt.return

  let works_lwt set =
    Lwt.bind set works

  let name_and_tunes set page =
    let versions =
      let%lwt versions_and_parameters = M.Set.versions_and_parameters set in
      let%lwt versions =
        Lwt_list.map_p
          (fun (version, _) -> Version.name version page)
          versions_and_parameters
      in
      versions
      |> List.flatten
      |> List.intertwine (fun _ -> (text " - " page :> Dom.node Js.t))
      |> Lwt.return
    in
    Lwt.return [
      (text_lwt (M.Set.name set) page :> Dom.node Js.t) ;
      (span ~classes:["details"] versions page :> Dom.node Js.t)
    ]

end

module Book = struct

  let title_and_subtitle book page =
    let%lwt subtitle_block =
      match%lwt M.Book.subtitle book with
      | "" -> Lwt.return_nil
      | subtitle -> Lwt.return [
          (span_static ~classes:["details"] [text subtitle page] page :> Dom.node Js.t)
        ]
    in
    Lwt.return (
      (text_lwt (M.Book.title book) page :> Dom.node Js.t)
      :: subtitle_block
    )

end
