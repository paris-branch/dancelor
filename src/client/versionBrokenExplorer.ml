open Js_of_ocaml
open Dancelor_client_elements

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in

  document##.title := js ("List of Broken Versions");

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
    h2 ~classes:["title"] [text "Hello"]
  ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
