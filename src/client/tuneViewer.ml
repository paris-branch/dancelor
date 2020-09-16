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
  let tune = Tune.get slug in
  let group = Lwt.bind tune Tune.group in
  let title = Text.Heading.h1 ~text:(Lwt.bind group TuneGroup.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let aka_text, kind_text, structure_text, key_text =
    let open Lwt in
    (match%lwt group >>= TuneGroup.alt_names with
     | [] -> Lwt.return ""
     | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return),
    (tune >>= fun tune ->
     group >>= Formatters.Kind.full_string tune >|= Printf.sprintf "Kind: %s"),
    (tune >>= Tune.structure >|= Printf.sprintf "Structure: %s"),
    (tune >>= Tune.key >|= Music.key_to_pretty_string >|= Printf.sprintf "Key: %s")
  in
  let aka, kind, structure, key =
    Text.Paragraph.create ~placeholder:"Also known as:" ~text:aka_text page,
    Text.Paragraph.create ~placeholder:"Kind:" ~text:kind_text page,
    Text.Paragraph.create ~placeholder:"Structure:" ~text:structure_text page,
    Text.Paragraph.create ~placeholder:"Key:" ~text:key_text page
  in
  Dom.appendChild content (Text.Paragraph.root aka);
  Dom.appendChild content (Text.Paragraph.root kind);
  Dom.appendChild content (Text.Paragraph.root structure);
  Dom.appendChild content (Text.Paragraph.root key);
  let pdf_href, ly_href =
    Helpers.build_path ~api:true ~route:(Router.TunePdf slug) (),
    Helpers.build_path ~api:true ~route:(Router.TuneLy slug) ()
  in
  let pdf, ly =
    Inputs.Button.create ~href:(Lwt.return pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
    Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
  in
  Dom.appendChild content (Inputs.Button.root pdf);
  Dom.appendChild content (Inputs.Button.root ly);
  Dom.appendChild content (Html.createHr document);
  let source =
    Printf.sprintf "/%s%s"
      Constant.api_prefix
      (Router.path_of_controller (Router.TuneSvg slug) |> snd) 
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
