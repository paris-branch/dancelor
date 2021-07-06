open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

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

           tr ~classes:["clickable"] [
             td [ a_lwt ~href_lwt ~classes:["fill"] (Formatters.Book.title_and_subtitle book) ];
             td [ a ~href_lwt ~classes:["fill"] [ text_lwt (let open Lwt in Book.date book >|= NesDate.to_string) ] ]
           ]
        )
        books
    )
  ]
