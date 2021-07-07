open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

let make books =
  DTable.map_table ~header:[ "Book"; "Date" ] books @@ fun book ->
  let href_lwt =
    let%lwt slug = Book.slug book in
    Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
  in
  DTable.clickable_row ~href_lwt [
    Formatters.Book.title_and_subtitle book;
    Lwt.return [ text_lwt (let open Lwt in Book.date book >|= NesDate.to_string) ]
  ]
