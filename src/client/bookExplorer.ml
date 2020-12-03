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
  let title = Html.createH2 document in
  title##.textContent := Js.some (js "All Books");
  Dom.appendChild content title;
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Books") ~text:(Lwt.return "Book") page;
        Table.Cell.header_text ~text:(Lwt.return "Date") page]
      page
  in
  let rows =
    let%lwt books = Book.get_all () in
    Lwt.return (List.map (fun book ->
      let href =
        let%lwt slug = Book.slug book in
        Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
      in
      let cells =
        let open Lwt in [
        Table.Cell.text ~text:(Book.title book) page;
        Table.Cell.text ~text:(Book.date book >|= NesDate.to_string) page]
      in
      Table.Row.create ~href ~cells page) books)
  in
  let section = Table.Section.create ~rows page in
  let table = Table.create
    ~kind:Table.Kind.Separated
    ~header
    ~contents:(Lwt.return [section])
    page
  in
  Dom.appendChild content (Table.root table);
  {page; content}

let contents t =
  t.content

let init t =
  ignore t

let refresh t =
  ignore t
