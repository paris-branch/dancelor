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

  let line = function
    | None -> Lwt.return ""
    | Some c -> M.Credit.line c

  let line_lwt credit =
    Lwt.bind credit line

end

module Tune = struct

  let description tune =
    (* FIXME: make author a link *)
    let%lwt kind = M.Tune.kind tune in
    let kind = M.Kind.base_to_pretty_string kind in
    let%lwt author = M.Tune.author tune in
    match author with
    | None ->
      Lwt.return (String.capitalize_ascii kind)
    | Some author when M.Credit.is_trad author ->
      Lwt.return ("Traditional " ^ kind)
    | Some author ->
      let%lwt line = M.Credit.line author in
      Lwt.return (String.capitalize_ascii kind ^ " by " ^ line)

  let description_lwt tune =
    Lwt.bind tune description

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

  let description version =
    (* FIXME: make arranger a link *)
    let%lwt bars = M.Version.bars version in
    let%lwt structure = M.Version.structure version in
    let%lwt key = M.Version.key version in
    let%lwt arranger =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return ""
      | Some arranger ->
        let%lwt line = M.Credit.line arranger in
        Lwt.return (spf " arranged by %s" line)
    in
    let%lwt disambiguation =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return ""
      | disambiguation ->
        Lwt.return (spf " (%s)" disambiguation)
    in
    spf "%d-bar %s version in %s%s%s"
      bars structure (M.Music.key_to_pretty_string key) arranger disambiguation
    |> Lwt.return

  let description_lwt version =
    Lwt.bind version description

  let name_and_disambiguation version page =
    let name =
      let%lwt tune = M.Version.tune version in
      M.Tune.name tune
    in
    let disambiguation =
      match%lwt M.Version.disambiguation version with
      | "" -> Lwt.return ""
      | disambiguation -> Lwt.return (spf " (%s)" disambiguation)
    in
    [
      (text_lwt name page :> Dom.node Js.t);
      (span_static ~classes:["dim"] [text_lwt disambiguation page] page :> Dom.node Js.t)
    ]

  let author_and_arranger ?(short=true) version page =
    let author =
      let%lwt tune = M.Version.tune version in
      match%lwt M.Tune.author tune with
      | None -> Lwt.return ""
      | Some author -> M.Credit.line author
    in
    let has_author =
      let%lwt tune = M.Version.tune version in
      match%lwt M.Tune.author tune with
      | None -> Lwt.return_false
      | Some _ -> Lwt.return_true
    in
    let arranged_by =
      match%lwt M.Version.arranger version with
      | None -> Lwt.return ""
      | Some arranger ->
        let%lwt comma = if%lwt has_author then Lwt.return ", " else Lwt.return "" in
        let arr = if short then "arr." else "arranged by" in
        let%lwt arranger = M.Credit.line arranger in
        Lwt.return (spf "%s%s %s" comma arr arranger)
    in
    [
      (text_lwt author page :> Dom.node Js.t);
      (span_static ~classes:["dim"] [text_lwt arranged_by page] page :> Dom.node Js.t)
    ]

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
      List.map
        (fun (version, _) ->
           let href =
             let%lwt slug = M.Version.slug version in
             Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
           in
           let name =
             let%lwt tune = M.Version.tune version in
             let%lwt name = M.Tune.name tune in
             Lwt.return (" - " ^ name)
           in
           link ~href [text_lwt name page] page)
        versions_and_parameters
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
