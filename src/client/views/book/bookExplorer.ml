open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    content : Html.divElement Js.t;
  }

let create page =
  let document = Page.document page in

  document##.title := js "All Books | Dancelor";

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
    let%lwt books = Book.search Formula.true_ >|=| Score.list_erase in
    Lwt.return (List.map (fun book ->
        let href =
          let%lwt slug = Book.slug book in
          Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
        in
        let cells =
          let open Lwt in [
            Table.Cell.create ~content:(
              let%lwt content = Formatters.Book.title_and_subtitle book in
              Lwt.return (Dancelor_client_html.nodes_to_dom_nodes document content)
            ) page;
            Table.Cell.text ~text:(Book.date book >|= NesDate.to_string) page
          ]
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
