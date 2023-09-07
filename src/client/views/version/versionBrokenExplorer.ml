open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in

  document##.title := js "Broken Versions | Dancelor";

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [text "List of Broken Versions"];

      div_lwt (
        let%lwt versions = Version.search Version.Filter.broken >|=| Score.list_erase in

        (* If there is no broken version, no need to print the table *)
        if List.length versions = 0 then
          Lwt.return [ text "No broken version" ]
        else
          Lwt.return [ Dancelor_client_tables.versions_with_names versions ]
      )
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
