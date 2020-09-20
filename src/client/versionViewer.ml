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
  let group = Lwt.bind version Version.group in
  let title = Text.Heading.h1 ~text:(Lwt.bind group Tune.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let aka_text, kind_text, author_text, arranger_text, structure_text, key_text, disambiguation_text =
    let open Lwt in
    (match%lwt group >>= Tune.alt_names with
     | [] -> Lwt.return ""
     | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return),
    (version >>= fun version ->
     group >>= Formatters.Kind.full_string version >|= Printf.sprintf "Kind: %s"),
    (group >>= Tune.author >>= Formatters.Credit.line >|= Printf.sprintf "Author: %s"),
    (match%lwt version >>= Version.arranger with
     | None -> Lwt.return ""
     | Some arranger -> Formatters.Credit.line (Some arranger) >|= Printf.sprintf "Arranger: %s"),
    (version >>= Version.structure >|= Printf.sprintf "Structure: %s"),
    (version >>= Version.key >|= Music.key_to_pretty_string >|= Printf.sprintf "Key: %s"),
    (version >>= Version.disambiguation >|= Printf.sprintf "Disambiguation: %s")
  in
  let aka, kind, author, arranger, structure, key, disambiguation =
    Text.Paragraph.create ~placeholder:"Also known as:" ~text:aka_text page,
    Text.Paragraph.create ~placeholder:"Kind:" ~text:kind_text page,
    Text.Paragraph.create ~placeholder:"Author: " ~text:author_text page,
    Text.Paragraph.create ~placeholder:"Arranger: " ~text:arranger_text page,
    Text.Paragraph.create ~placeholder:"Structure:" ~text:structure_text page,
    Text.Paragraph.create ~placeholder:"Key:" ~text:key_text page,
    Text.Paragraph.create ~placeholder:"Disambiguation:" ~text:disambiguation_text page
  in
  Dom.appendChild content (Text.Paragraph.root aka);
  Dom.appendChild content (Text.Paragraph.root kind);
  Dom.appendChild content (Text.Paragraph.root author);
  Dom.appendChild content (Text.Paragraph.root arranger);
  Dom.appendChild content (Text.Paragraph.root structure);
  Dom.appendChild content (Text.Paragraph.root key);
  Dom.appendChild content (Text.Paragraph.root disambiguation);
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
    let%lwt slug = Lwt.bind group Tune.slug in
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
