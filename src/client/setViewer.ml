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
  versions : Html.uListElement Js.t;
}

let display_versions t versions =
  t.versions##.textContent := Js.null;
  List.iter (fun version ->
    let group = Version.group version in
    let slug = Version.slug version in
    let href =
      let%lwt slug = slug in
      Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
    in
    let name = Lwt.bind group Tune.name in
    let title = Text.Link.create ~href ~text:name t.page in
    let source =
      Lwt.map (fun slug ->
        Printf.sprintf "/%s%s"
          Constant.api_prefix
          (Router.path_of_controller (Router.VersionSvg slug) |> snd))
        slug
    in
    let img = Image.create ~source t.page in
    let li = Html.createLi (Page.document t.page) in
    Dom.appendChild li (Text.Link.root title);
    Dom.appendChild li (Image.root img);
    Dom.appendChild t.versions li)
    versions

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let set = Set.get slug in
  let title = Text.Heading.h1 ~text:(Lwt.bind set Set.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let deviser_text, kind_text =
    let open Lwt in
    (set >>= Set.deviser >>= Formatters.Credit.line >|= Printf.sprintf "Deviser: %s"),
    (set >>= Set.kind >|= Kind.dance_to_string >|= Printf.sprintf "Kind: %s")
  in
  let deviser, kind =
    Text.Paragraph.create ~placeholder:"Deviser:" ~text:deviser_text page,
    Text.Paragraph.create ~placeholder:"Kind:" ~text:kind_text page
  in
  Dom.appendChild content (Text.Paragraph.root deviser);
  Dom.appendChild content (Text.Paragraph.root kind);
  let c_pdf_href, b_pdf_href, e_pdf_href, ly_href =
    Helpers.build_path ~api:true ~route:(Router.SetPdf slug) (),
    Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
      ~query:["transpose-target", ["bes"]] (),
    Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
      ~query:["transpose-target", ["ees"]] (),
    Helpers.build_path ~api:true ~route:(Router.SetLy slug) ()
  in
  let c_pdf, b_pdf, e_pdf, ly =
    Inputs.Button.create ~href:(Lwt.return c_pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
    Inputs.Button.create ~href:(Lwt.return b_pdf_href) ~icon:"file-pdf" ~text:"PDF (Bb)" page,
    Inputs.Button.create ~href:(Lwt.return e_pdf_href) ~icon:"file-pdf" ~text:"PDF (Eb)" page,
    Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
  in
  Dom.appendChild content (Inputs.Button.root c_pdf);
  Dom.appendChild content (Inputs.Button.root b_pdf);
  Dom.appendChild content (Inputs.Button.root e_pdf);
  Dom.appendChild content (Inputs.Button.root ly);
  Dom.appendChild content (Html.createHr document);
  let prev_title = Text.Heading.h2 ~text:(Lwt.return "Previsualisation") page in
  Dom.appendChild content (Text.Heading.root prev_title);
  let versions = Html.createUl (Page.document page) in
  versions##.textContent := Js.some (js "Loading versions...");
  Dom.appendChild content versions;
  let t = {page; content; versions} in
  Lwt.on_success set (fun set -> Lwt.on_success (Set.versions set) (display_versions t));
  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
