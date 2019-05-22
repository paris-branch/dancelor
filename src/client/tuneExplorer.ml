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

let create page = 
  let document = Page.document page in
  let content = Html.createDiv document in
  let title = Html.createH1 document in
  title##.textContent := Js.some (js "All Tunes");
  Dom.appendChild content title;
  let header = 
    Table.Row.create 
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Key") page;
        Table.Cell.header_text ~text:(Lwt.return "Structure") page;
        Table.Cell.header_text ~width:"30%" ~text:(Lwt.return "Author") page] 
      page
  in
  let rows = 
    let%lwt tunes = Tune.get_all () in
    Lwt.return (List.map (fun score -> 
      let tune = Score.value score in
      let href = 
        let%lwt slug = Tune.slug tune in
        Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd) 
      in
      let cells = 
        let group = Tune.group tune in 
        let open Lwt in [
        Table.Cell.link ~href ~text:(group >>= TuneGroup.name) page;
        Table.Cell.text ~text:(group >>= Formatters.Kind.full_string tune) page;
        Table.Cell.text ~text:(Tune.key tune >|= Music.key_to_string) page;
        Table.Cell.text ~text:(Tune.structure tune) page;
        Table.Cell.text ~text:(group >>= TuneGroup.author >>= Formatters.Credit.line) page]
      in
      Table.Row.create ~href ~cells page) tunes)
  in
  let table = Table.create
    ~header
    ~contents:rows
    page
  in
  Dom.appendChild content (Table.root table);
  {page; content}

let contents t =
  t.content
