open Dancelor_common
open Dancelor_client_model
open Dancelor_client_elements
module Formatters = Dancelor_client_formatters

let make sets_lwt page =

  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Sets") ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Deviser") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Actions") page]
      page
  in
  let table = Table.create ~header ~kind:Table.Kind.Separated page in

  let rows =
    let%lwt sets = sets_lwt in
    List.map
      (fun set ->
         let href =
           let%lwt slug = Set.slug set in
           Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
         in
         let cells =
           let open Lwt in [
             Table.Cell.create ~content:(Formatters.Set.name_and_tunes set page) page;
             Table.Cell.create ~content:(
               let%lwt deviser = Set.deviser set in
               Formatters.Credit.line deviser page
             ) page;
             Table.Cell.text ~text:(Set.kind set >|= Kind.dance_to_string) page;
             Table.Cell.text ~text:(Lwt.return "") page]
         in
         Table.Row.create ~href ~cells page)
      sets
    |> Lwt.return
  in
  let section = Table.Section.create ~rows page in
  Table.replace_bodies table (Lwt.return [section]);

  table
