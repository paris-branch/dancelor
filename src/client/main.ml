open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_views

module Html = Dom_html

let js = Js.string

let on_load _ev =
  let page = Page.create () in
  let header = Header.create page in
  Header.add_menu_entry header "Search" PageRouter.(path (Search None));
  Header.add_dropdown_menu_entry
    header
    "Explore"
    [
      "Tunes", PageRouter.(path VersionAll);
      "Sets", PageRouter.(path SetAll);
      "Books", PageRouter.(path BookAll);
    ];
  Header.add_dropdown_menu_entry
    header
    "Add"
    [
      "Tune", PageRouter.(path VersionAdd);
      "Set", PageRouter.(path SetCompose);
      "Book", PageRouter.(path BookCompose);
    ];
  Page.set_header page (Header.contents header);
  let url =
    Html.window##.location##.href
    |> Js.to_string
    |> Uri.of_string
  in
  let module M = (val (Dispatcher.dispatch url) : Page.CONTENTS) in
  let contents = M.create page in
  Page.set_contents (module M) page contents;
  M.init contents;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
