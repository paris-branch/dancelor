open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_common

module Html = Dom_html

let js = Js.string

let on_load _ev =
  let page = Page.create () in
  let header = Header.create page in
  Header.add_menu_entry header "Tunes" (Router.path_of_controller Router.TuneAll |> snd);
  Header.add_menu_entry header "Sets" (Router.path_of_controller Router.SetAll |> snd);
  Header.add_menu_entry header "Programs" (Router.path_of_controller Router.ProgramAll |> snd);
  Header.add_menu_entry header "Compose a Set" (Router.path_of_controller Router.SetCompose |> snd);
  Page.set_header page (Header.contents header);
  let contents = Dispatcher.get_contents page in
  Page.set_contents page contents;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
