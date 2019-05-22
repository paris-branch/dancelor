open Js_of_ocaml
open Dancelor_client_elements

module Html = Dom_html

let js = Js.string

let on_load _ev =
  let page = Page.create () in
  let header = Header.create page in
  Header.add_menu_entry header "Tunes" "/tune/all";
  Header.add_menu_entry header "Sets" "/set/all";
  Header.add_menu_entry header "Programs" "/program/all";
  Header.add_menu_entry header "Compose a Set" "/set/compose";
  Page.set_header page (Header.contents header);
  let contents = Dispatcher.get_contents page in
  Page.set_contents page contents;
  let footer = Footer.create () in
  Page.set_footer page (Footer.contents footer);
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
