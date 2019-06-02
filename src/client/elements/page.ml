open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type t =
{
  document : Html.document Js.t;
  body : Html.bodyElement Js.t;
  header : Html.element Js.t;
  mutable content : (Html.divElement Js.t) option;
}

let create () =
  let document = Html.window##.document in
  let body = document##.body in
  let header = document##createElement (js "header") in
  Dom.appendChild body header;
  {document; body; header; content = None}

let document t =
  t.document

let set_header t contents =
  JsHelpers.clear_children t.header;
  Dom.appendChild t.header contents

let set_contents t contents =
  begin match t.content with
  | None -> Dom.appendChild t.body contents
  | Some c -> Dom.replaceChild t.body c contents
  end;
  contents##.classList##add (js "content");
  contents##.classList##add (js "page-body");
  t.content <- Some contents
