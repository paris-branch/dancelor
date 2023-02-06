open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_utils
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t = {
  page: Page.t;
  content: Html.divElement Js.t;
  page_nav: PageNav.t;
  table_wrapper: Html.divElement Js.t;
  error_wrapper: Html.divElement Js.t;
  table: Table.t;
  error: Html.divElement Js.t;
  bar: Inputs.Text.t;
}

let update_table t =
  let pagination = PageNav.pagination t.page_nav in
  let input = Inputs.Text.contents t.bar in
  (* we can afford the exception because we'll have checked before *)
  let filter = AnyFilter.from_string_exn input in
  let rows =
    let%lwt results = Any.search ~pagination filter in
    Lwt_list.map_s (AnyResult.make_result t.page) results
  in
  let section = Table.Section.create ~rows t.page in
  Table.replace_bodies t.table (Lwt.return [section])

let update_title_and_uri t =
  (* Update title *)
  let input = Inputs.Text.contents t.bar in
  let for_ =
    if input = "" then ""
    else spf " for %s" input
  in
  (Page.document t.page)##.title := js (spf "Magic Search%s | Dancelor" for_);
  (* Update URI *)
  Dom_html.window##.history##replaceState
    "fixme-the-state"
    (js "")
    (Js.some (js (spf "/search?q=%s" (Yojson.Safe.to_string (`String input)))))

let update t =
  update_title_and_uri t;
  let input = Inputs.Text.contents t.bar in
  match AnyFilter.from_string input with
  | Ok filter ->
    (t.table_wrapper##.style##.display := js "block";
    t.error_wrapper##.style##.display := js "none";
    Lwt.on_success (Any.count filter) (PageNav.set_entries t.page_nav);
    update_table t)
  | Error msgs ->
    (t.table_wrapper##.style##.display := js "none";
    t.error_wrapper##.style##.display := js "block";
    JsHelpers.clear_children t.error;
    let ul = Html.createUl (Page.document t.page) in
    List.iter
      (fun msg ->
        let li = Html.createLi (Page.document t.page) in
        li##.textContent := Js.some (js msg);
        Dom.appendChild ul li)
      msgs;
    Dom.appendChild t.error ul)

let create input page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let bar =
    Inputs.Text.create
      ?default: input
      ~placeholder: "Search for anything (it really is magic!)"
      page
  in
  let header =
    Table.Row.create
      ~cells: [
        Table.Cell.header_text ~text: (Lwt.return "Score") page;
        Table.Cell.header_text ~text: (Lwt.return "Type") page;
        Table.Cell.header_text ~text: (Lwt.return "Name") page;
        Table.Cell.header_text ~text: (Lwt.return "Kind") page;
        Table.Cell.header_text ~text: (Lwt.return "By") page;
      ]
      page
  in
  let table =
    Table.create
      ~header
      ~kind: Table.Kind.Separated
      page
  in
  let table_wrapper = Html.createDiv document in
  let error_wrapper = Html.createDiv document in
  error_wrapper##.classList##add (js "error");
  let error = Html.createDiv document in
  Dom.appendChild error_wrapper error;
  let page_nav = PageNav.create ~entries: 0 ~entries_per_page: 25 page in
  let t = { page; content; page_nav; table_wrapper; error_wrapper; table; error; bar } in
  let () =
    let title = Text.Heading.h2_static ~text: (Lwt.return "Magic Search") page in
    Dom.appendChild content (Text.Heading.root title);
  in
  let () =
    Inputs.Text.on_change bar (fun _ -> update t);
    Dom.appendChild content (Inputs.Text.root bar);
  in
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild table_wrapper (Table.root table);
  Dom.appendChild table_wrapper (PageNav.root page_nav);
  Dom.appendChild content table_wrapper;
  Dom.appendChild content error_wrapper;
  PageNav.connect_on_page_change
    page_nav
    (fun _ ->
      PageNav.rebuild page_nav;
      update_table t;
      (Inputs.Text.root t.bar)##focus);
  update t;
  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  (Inputs.Text.root t.bar)##focus
