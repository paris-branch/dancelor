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
  sets : Html.uListElement Js.t;
}

let display_sets t sets = 
  t.sets##.textContent := Js.null;
  List.iter (fun set ->
    let slug = Set.slug set in
    let href = 
      let%lwt slug = slug in
      Lwt.return (Router.path_of_controller (Router.Set slug) |> snd) 
    in
    let name = Set.name set in
    let link = Text.Link.create ~href ~text:name t.page in
    let li = Html.createLi (Page.document t.page) in
    Dom.appendChild li (Text.Link.root link);
    Dom.appendChild t.sets li)
    sets

let create page slug = 
  let document = Page.document page in
  let content = Html.createDiv document in
  let program = Program.get slug in
  let title = Text.Heading.h1 ~text:(Lwt.bind program Program.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let date_text = 
    let open Lwt in
    (program >>= Program.date >|= NesDate.to_string >|= Printf.sprintf "Date: %s")
  in
  let date = Text.Paragraph.create ~placeholder:"Date:" ~text:date_text page in
  Dom.appendChild content (Text.Paragraph.root date);
  Dom.appendChild content (Html.createHr document);
  let prev_title = Text.Heading.h2 ~text:(Lwt.return "Sets") page in
  Dom.appendChild content (Text.Heading.root prev_title);
  let sets = Html.createUl (Page.document page) in
  sets##.textContent := Js.some (js "Loading sets...");
  Dom.appendChild content sets;
  let t = {page; content; sets} in
  Lwt.on_success program (fun prog -> Lwt.on_success (Program.sets prog) (display_sets t));
  t

let contents t =
  t.content
