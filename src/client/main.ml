open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_common
open Dancelor_client_views

module Html = Dom_html

let js = Js.string

let on_load _ev =
  let page = Page.create () in
  let header = Header.create page in
  Header.add_menu_entry header "Magic Search" (Router.path_of_controller Router.MagicSearch |> snd);
  Header.add_dropdown_menu_entry
    header
    "Explore"
    [
      "Tunes", (Router.path_of_controller Router.VersionAll |> snd);
      "Sets", (Router.path_of_controller Router.SetAll |> snd);
      "Books", (Router.path_of_controller Router.BookAll |> snd);
    ];
  Header.add_dropdown_menu_entry
    header
    "Add"
    [
      "Tune", (Router.path_of_controller Router.VersionAddition |> snd);
      "Set", (Router.path_of_controller Router.SetCompose |> snd);
      "Book", (Router.path_of_controller Router.BookCompose |> snd);
    ];
  Page.set_header page (Header.contents header);
  let url =
    Html.window##.location##.href
    |> Js.to_string
    |> Uri.of_string
  in
  let module M = (val (Dispatcher.dispatch url) : Page.CONTENTS)
  in
  let contents = M.create page in
  Page.set_contents (module M) page contents;
  M.init contents;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
