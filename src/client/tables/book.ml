open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

let clickable_row ?href ?href_lwt cells =
  tr ~classes:["clickable"] (
    List.map
      (fun cell_lwt ->
         td [ a_lwt ~classes:["fill"] ?href ?href_lwt cell_lwt ])
      cells
  )

let make books =
  table ~classes:["separated-table"; "visible"] [
    thead [
      tr [
        td [ text "Book" ];
        td [ text "Date" ];
      ]
    ];

    tbody (
      List.map
        (fun book ->
           let href_lwt =
             let%lwt slug = Book.slug book in
             Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
           in

           clickable_row ~href_lwt [
             Formatters.Book.title_and_subtitle book;
             Lwt.return [ text_lwt (let open Lwt in Book.date book >|= NesDate.to_string) ]
           ]
        )
        books
    )
  ]
