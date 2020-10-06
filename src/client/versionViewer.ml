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
    let text = Formatters.Tune.aka_lwt tune in
    let aka = Text.Heading.h3 ~text page in
    Dom.appendChild content (Text.Heading.root aka)
  in

  (* tune description *)
  let () =
    let text = Formatters.Tune.description_lwt tune in
    let tune_description = Text.Heading.h3 ~text page in
    Dom.appendChild content (Text.Heading.root tune_description)
  in

  (* version description *)
  let () =
    let text = Formatters.Version.description_lwt version in
    let version_description = Text.Heading.h3 ~text page in
    Dom.appendChild content (Text.Heading.root version_description)
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
