open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  content : Html.divElement Js.t;
  page_nav : PageNav.t;
  table : Table.t;
  bar : Inputs.Text.t;
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

let update_title t =
  (* FIXME: will only make sense when the URL also changes *)
  (* let input = Inputs.Text.contents t.bar in *)
  (* let for_ = *)
  (*   if input = "" then "" *)
  (*   else spf " for %s" input *)
  (* in *)
  let for_ = "" in
  (Page.document t.page)##.title := js (spf "Magic Search%s | Dancelor" for_)

let update t =
  update_title t;
  let input = Inputs.Text.contents t.bar in
  match AnyFilter.from_string input with
  | Ok filter ->
    (
      Lwt.on_success (Any.count filter) (PageNav.set_entries t.page_nav);
      update_table t
    )
  | Error _err ->
    (
      Format.printf "FIXME: HANDLE ERRORS@."
    ) (* FIXME *)

let create page =
  let document = Page.document page in
  let content = Html.createDiv document in

  let bar =
    Inputs.Text.create
      ~default:"Search for anything (it really is magic!)"
      page
  in

  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~text:(Lwt.return "Score") page;
        Table.Cell.header_text ~text:(Lwt.return "Type") page;
        Table.Cell.header_text ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "By") page
      ]
      page
  in
  let table = Table.create
    ~header
    ~kind:Table.Kind.Separated
    page
  in

  let page_nav = PageNav.create ~entries:0 ~entries_per_page:25 page in

  let t = { page ; content ; page_nav ; table ; bar } in

  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.return "Magic Search") page in
    Dom.appendChild content (Text.Heading.root title);
  in

  let () =
    Inputs.Text.on_change bar (fun _ -> update t);
    Dom.appendChild content (Inputs.Text.root bar);
  in

  Dom.appendChild content (Html.createHr document);

  Dom.appendChild content (Table.root table);
  Dom.appendChild content (PageNav.root page_nav);

  PageNav.connect_on_page_change page_nav (fun _ ->
      PageNav.rebuild page_nav;
      update_table t);
  update t;
  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
