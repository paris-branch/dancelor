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
  tunes : Html.uListElement Js.t;
}

let display_tunes t tunes = 
  t.tunes##.textContent := Js.null;
  List.iter (fun tune ->
    let group = Tune.group tune in
    let slug = Tune.slug tune in
    let href = 
      let%lwt slug = slug in
      Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd) 
    in
    let name = Lwt.bind group TuneGroup.name in
    let title = Text.Link.create ~href ~text:name t.page in
    let source = 
      Lwt.map (fun slug ->
        Printf.sprintf "/%s%s" 
          Constant.api_prefix 
          (Router.path_of_controller (Router.TunePng slug) |> snd))
        slug
    in
    let img = Image.create ~source t.page in
    let li = Html.createLi (Page.document t.page) in
    Dom.appendChild li (Text.Link.root title);
    Dom.appendChild li (Image.root img);
    Dom.appendChild t.tunes li)
    tunes

let create page slug = 
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
    Dancelor_client_api.build_path ~api:true ~route:(Router.SetPdf slug) (),
    Dancelor_client_api.build_path ~api:true ~route:(Router.SetPdf slug) 
      ~query:["transpose-target", ["bes"]] (),
    Dancelor_client_api.build_path ~api:true ~route:(Router.SetPdf slug) 
      ~query:["transpose-target", ["ees"]] (),
    Dancelor_client_api.build_path ~api:true ~route:(Router.SetLy slug) ()
  in
  let c_pdf, b_pdf, e_pdf, ly = 
    Text.Link.create ~href:(Lwt.return c_pdf_href) ~text:(Lwt.return "Link to the PDF") page,
    Text.Link.create ~href:(Lwt.return b_pdf_href) ~text:(Lwt.return "B flat version") page,
    Text.Link.create ~href:(Lwt.return e_pdf_href) ~text:(Lwt.return "E flat version") page,
    Text.Link.create ~href:(Lwt.return ly_href) ~text:(Lwt.return "Link to the Lilypond") page
  in
  Dom.appendChild content (Text.Link.root c_pdf);
  (Page.document page)##createTextNode (js " (") |> Dom.appendChild content;
  Dom.appendChild content (Text.Link.root b_pdf);
  (Page.document page)##createTextNode (js ") (") |> Dom.appendChild content;
  Dom.appendChild content (Text.Link.root e_pdf);
  (Page.document page)##createTextNode (js ") ") |> Dom.appendChild content;
  Dom.appendChild content (Text.Link.root ly);
  Dom.appendChild content (Html.createHr document);
  let prev_title = Text.Heading.h2 ~text:(Lwt.return "Previsualisation") page in
  Dom.appendChild content (Text.Heading.root prev_title);
  let tunes = Html.createUl (Page.document page) in
  tunes##.textContent := Js.some (js "Loading tunes...");
  Dom.appendChild content tunes;
  let t = {page; content; tunes} in
  Lwt.on_success set (fun set -> Lwt.on_success (Set.tunes set) (display_tunes t));
  t

let contents t =
  t.content
