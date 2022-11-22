open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_views
module Router = Dancelor_client_router

module Html = Dom_html

let js = Js.string

let on_load _ev =
  let page = Page.create () in
  let header = Header.create page in
  Header.add_menu_entry header "Search" Router.(path (Search None));
  Header.add_dropdown_menu_entry header "Explore" [
    "Tunes", Router.(path VersionAll);
    "Sets",  Router.(path SetAll);
    "Books", Router.(path BookAll);
  ];
  Header.add_dropdown_menu_entry header "Add" [
    "Tune", Router.(path VersionAdd);
    "Set",  Router.(path SetCompose);
    "Book",  Router.(path BookCompose);
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
