open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
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
  let content = Html.createDiv document in

  document##.title := js "All books | Dancelor";

  (
    let open Dancelor_client_html.NewAPI in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "All books"];

      tablex
        ~a:[a_class ["separated-table"]]
        ~thead:(
          thead [
            tr [
              th [
                span ~a:[a_class ["full-content"]] [txt "Book"];
                span ~a:[a_class ["collapse-content"]] [txt "Books"];
              ];
              th [txt "Date"];
            ]
          ]
        )
        [
          L.tbody (
            Fun.flip Lwt.map
              (Book.search Formula.true_ >|=| Score.list_erase)
              (List.map
                 (fun book ->
                    let href =
                      let%lwt slug = Book.slug book in
                      Lwt.return PageRouter.(path (Book slug))
                    in
                    Dancelor_client_tables.TheNewAPI.clickable_row ~href [
                      (Formatters.BookNewAPI.title_and_subtitle book);
                      (Lwt.map
                         (List.singleton % txt % Option.fold ~none:"" ~some:NesPartialDate.to_pretty_string)
                         (Book.date book));
                    ]
                 )
              )
          )
        ];
    ]
  );

  {page; content}

let contents t =
  t.content

let init t =
  ignore t

let refresh t =
  ignore t
