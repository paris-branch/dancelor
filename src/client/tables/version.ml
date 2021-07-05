open Dancelor_common
open Dancelor_client_model
open Dancelor_client_elements
module Formatters = Dancelor_client_formatters

let make versions_lwt page =

  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~text:(Lwt.return "Disambiguation") page;
        Table.Cell.header_text ~text:(Lwt.return "Arranger") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Key") page;
        Table.Cell.header_text ~text:(Lwt.return "Structure") page
      ]
      page
  in

  let rows =
    let%lwt versions = versions_lwt in
    List.map
      (fun version ->
         let href =
           let%lwt slug = Version.slug version in
           Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
         in
         let cells =
           let tune = Version.tune version in
           let open Lwt in [
             Table.Cell.text ~text:(Version.disambiguation version) page;
             Table.Cell.create ~content:(
               let%lwt arranger = Version.arranger version in
               let%lwt content = Formatters.Credit.line arranger in
               Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
             ) page;
             Table.Cell.create ~content:(
               let%lwt content = tune >>= Formatters.Kind.full_string version in
               Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
             ) page;
             Table.Cell.text ~text:(Version.key version >|= Music.key_to_pretty_string) page;
             Table.Cell.text ~text:(Version.structure version) page;
           ]
         in
         Table.Row.create ~href ~cells page)
      versions
    |> Lwt.return
  in

  let table = Table.create ~kind:Table.Kind.Separated ~header page in
  let section = Table.Section.create ~rows page in
  Table.replace_bodies table (Lwt.return [section]);
  table
