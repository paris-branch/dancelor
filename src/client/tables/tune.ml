open Dancelor_common
open Dancelor_client_model
open Dancelor_client_elements
module Formatters = Dancelor_client_formatters

let make tunes_lwt page =

  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Author") page
      ]
      page
  in

  let rows =
    let%lwt tunes = tunes_lwt in
    List.map
      (fun tune ->
         let href =
           let%lwt slug = Tune.slug tune in
           Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
         in
         let cells =
           let open Lwt in [
             Table.Cell.text ~text:(Tune.name tune) page ;
             Table.Cell.text ~text:(Tune.kind tune >|= Kind.base_to_pretty_string ~capitalised:true) page ;
             Table.Cell.create ~content:(
               let%lwt author = Tune.author tune in
               let%lwt content = Formatters.Credit.line author in
               Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
             ) page ;
           ]
         in
         Table.Row.create ~href ~cells page)
      tunes
    |> Lwt.return
  in

  let table = Table.create ~kind:Table.Kind.Separated ~header page in
  let section = Table.Section.create ~rows page in
  Table.replace_bodies table (Lwt.return [section]);
  table
