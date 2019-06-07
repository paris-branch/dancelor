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
  let title = Html.createH1 document in
  title##.textContent := Js.some (js "All Programs");
  Dom.appendChild content title;
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Programs") ~text:(Lwt.return "Program") page;
        Table.Cell.header_text ~text:(Lwt.return "Date") page]
      page
  in
  let rows =
    let%lwt programs = Program.get_all () in
    let programs = List.sort (fun p1 p2 -> NesDate.compare (Program.date p1) (Program.date p2)) programs in
    Lwt.return (List.map (fun program ->
      let href =
        let%lwt slug = Program.slug program in
        Lwt.return (Router.path_of_controller (Router.Program slug) |> snd)
      in
      let cells =
        let open Lwt in [
        Table.Cell.link ~href ~text:(Program.name program) page;
        Table.Cell.text ~text:(Program.date program >|= NesDate.to_string) page]
      in
      Table.Row.create ~href ~cells page) programs)
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
