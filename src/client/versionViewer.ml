open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  content : Html.divElement Js.t;
}

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let version = Version.get slug in
  let tune = Lwt.bind version Version.tune in

  (* title *)
  let () =
    let title = Text.Heading.h2 ~text:(Lwt.bind tune Tune.name) page in
    Dom.appendChild content (Text.Heading.root title)
  in

  (* aka *)
  let () =
    let aka_text =
      match%lwt Lwt.bind tune Tune.alt_names with
      | [] -> Lwt.return ""
      | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return
    in
    let aka = Text.Heading.h3 ~text:aka_text page in
    Dom.appendChild content (Text.Heading.root aka)
  in

  (* kind and author *)
  let () =
    let open Lwt in
    let kind_by_author_text =
      let%lwt kind = tune >>= Tune.kind in
      let kind = Kind.base_to_pretty_string kind in
      let%lwt author = tune >>= Tune.author in
      match author with
      | None ->
        Lwt.return (String.capitalize_ascii kind)
      | Some author when Credit.is_trad author ->
        Lwt.return ("Traditional " ^ kind)
      | Some author ->
        let%lwt line = Credit.line author in
        Lwt.return (String.capitalize_ascii kind ^ " by " ^ line)
    in
    let kind_by_author = Text.Heading.h3 ~text:kind_by_author_text page in
    Dom.appendChild content (Text.Heading.root kind_by_author)
  in

  (* version details *)
  let () =
    let open Lwt in
    let version_details_text =
      let%lwt bars = version >>= Version.bars in
      let%lwt structure = version >>= Version.structure in
      let%lwt key = version >>= Version.key in
      let%lwt arranger =
        match%lwt version >>= Version.arranger with
        | None -> Lwt.return ""
        | Some arranger ->
          Formatters.Credit.line (Some arranger)
          >|= Format.sprintf " arranged by %s"
      in
      let%lwt disambiguation =
        match%lwt version >>= Version.disambiguation with
        | "" -> Lwt.return ""
        | disambiguation ->
          Format.sprintf " (%s)" disambiguation
          |> Lwt.return
      in
      Format.sprintf "%d-bar %s version in %s%s%s"
        bars structure (Music.key_to_pretty_string key) arranger disambiguation
    |> Lwt.return
    in
    let version_details = Text.Heading.h3 ~text:version_details_text page in
    Dom.appendChild content (Text.Heading.root version_details)
  in

  let pdf_href, ly_href =
    Helpers.build_path ~api:true ~route:(Router.VersionPdf slug) (),
    Helpers.build_path ~api:true ~route:(Router.VersionLy slug) ()
  in
  let pdf, ly =
    Inputs.Button.create ~href:(Lwt.return pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
    Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
  in
  Dom.appendChild content (Inputs.Button.root pdf);
  Dom.appendChild content (Inputs.Button.root ly);
  Dom.appendChild content (Html.createHr document);

  let href =
    let%lwt slug = Lwt.bind tune Tune.slug in
    Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
  in
  let title = Text.Link.create ~href ~text:(Lwt.return "See all versions") page in
  Dom.appendChild content (Text.Link.root title);

  let source =
    Printf.sprintf "/%s%s"
      Constant.api_prefix
      (Router.path_of_controller (Router.VersionSvg slug) |> snd)
    |> Lwt.return
  in
  let img = Image.create ~source page in
  Dom.appendChild content (Image.root img);
  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
