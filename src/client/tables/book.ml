open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common
module Formatters = Dancelor_client_formatters

let make books_lwt page =
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Books") ~text:(Lwt.return "Book") page;
        Table.Cell.header_text ~text:(Lwt.return "Date") page]
      page
  in

  let rows =
    let%lwt books = books_lwt in
    List.map
      (fun book ->
         let href =
           let%lwt slug = Book.slug book in
           Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
         in
         let cells =
           let open Lwt in [
             Table.Cell.create ~content:(Formatters.Book.title_and_subtitle book page) page;
             Table.Cell.text ~text:(Book.date book >|= NesDate.to_string) page
           ]
         in
         Table.Row.create ~href ~cells page)
      books
    |> Lwt.return
  in

  let section = Table.Section.create ~rows page in
  let table = Table.create
      ~kind:Table.Kind.Separated
      ~header
      ~contents:(Lwt.return [section])
      page
  in

  table
