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

let create page slug = 
  let document = Page.document page in
  let content = Html.createDiv document in
  let tune = Tune.get slug in
  let group = Lwt.bind tune Tune.group in
  let title = Text.Heading.h1 ~text:(Lwt.bind group TuneGroup.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let kind_text, structure_text, key_text = 
    let open Lwt in
    (tune >>= fun tune -> 
     group >>= Formatters.Kind.full_string tune >|= Printf.sprintf "Kind: %s"),
    (tune >>= Tune.structure >|= Printf.sprintf "Structure: %s"),
    (tune >>= Tune.key >|= Music.key_to_string >|= Printf.sprintf "Key: %s")
  in
  let kind, structure, key = 
    Text.Paragraph.create ~placeholder:"Kind:" ~text:kind_text page,
    Text.Paragraph.create ~placeholder:"Structure:" ~text:structure_text page,
    Text.Paragraph.create ~placeholder:"Key:" ~text:key_text page
  in
  Dom.appendChild content (Text.Paragraph.root kind);
  Dom.appendChild content (Text.Paragraph.root structure);
  Dom.appendChild content (Text.Paragraph.root key);
  let ly_href = Helpers.build_path ~api:true ~route:(Router.TuneLy slug) () in
  let ly = 
    Text.Link.create ~href:(Lwt.return ly_href) ~text:(Lwt.return "Link to the Lilypond") page
  in
  Dom.appendChild content (Text.Link.root ly);
  Dom.appendChild content (Html.createHr document);
  let source = 
    Printf.sprintf "/%s%s" 
      Constant.api_prefix 
      (Router.path_of_controller (Router.TunePng slug) |> snd) 
    |> Lwt.return
  in
  let img = Image.create ~source page in
  Dom.appendChild content (Image.root img);
  {page; content}

let contents t =
  t.content
