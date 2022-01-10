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
  page_nav : PageNav.t;
  table : Table.t;
}

let update_table t =
  let rows =
    let pagination = PageNav.pagination t.page_nav in
    Lwt.map
      (List.map
         (fun set ->
            let href =
              let%lwt slug = Set.slug set in
              Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
            in
            let cells =
              let open Lwt in [
                Table.Cell.create ~content:(
                  let%lwt content = Formatters.Set.name_and_tunes ~link:false set in
                  Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document t.page) content)
                ) t.page;
                Table.Cell.create ~content:(
                  let%lwt deviser = Set.deviser set in
                  let%lwt content = Formatters.Credit.line deviser in
                  Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document t.page) content)
                ) t.page;
                Table.Cell.text ~text:(Set.kind set >|= Kind.dance_to_string) t.page;
                Table.Cell.text ~text:(Lwt.return "") t.page
              ]
            in
            Table.Row.create ~href ~cells t.page))
      (Set.search ~pagination Formula.true_
       >|=| Score.list_erase)
  in
  let section = Table.Section.create ~rows t.page in
  Table.replace_bodies t.table (Lwt.return [section])

let create page =
  let document = Page.document page in

  document##.title := js "All Sets | Dancelor";

  let content = Html.createDiv document in
  let title = Html.createH2 document in
  title##.textContent := Js.some (js "All Sets");
  title##.classList##add (js "title");
  Dom.appendChild content title;

  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Sets") ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Deviser") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Actions") page]
      page
  in
  let table = Table.create
    ~header
    ~kind:Table.Kind.Separated
    page
  in
  Dom.appendChild content (Table.root table);
  let page_nav = PageNav.create ~entries:0 ~entries_per_page:25 page in
  Dom.appendChild content (PageNav.root page_nav);
  let t = {page; content; table; page_nav} in
  PageNav.connect_on_page_change page_nav (fun _ ->
    PageNav.rebuild page_nav;
    update_table t);
  Lwt.on_success (Set.count Formula.true_) (fun entries ->
    PageNav.set_entries page_nav entries);
  update_table t;
  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
