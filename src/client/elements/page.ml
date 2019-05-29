open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type t =
{
  document : Html.document Js.t;
  body : Html.bodyElement Js.t;
  header : Html.element Js.t;
  content : Html.divElement Js.t;
}

let create () =
  let document = Html.window##.document in
  let body = document##.body in
  let header = document##createElement (js "header") in
  let content = Html.createDiv document in
  content##.id := js "content";
  content##.classList##add (js "content");
  Dom.appendChild body header;
  Dom.appendChild body content;
  {document; body; header; content}

let document t =
  t.document

let set_header t contents =
  while Js.to_bool t.header##hasChildNodes do
    let child = Js.Opt.get t.header##.firstChild (fun () -> assert false) in
    t.header##removeChild child |> ignore
  done;
  Dom.appendChild t.header contents

let set_contents t contents =
  while Js.to_bool t.content##hasChildNodes do
    let child = Js.Opt.get t.content##.firstChild (fun () -> assert false) in
    t.content##removeChild child |> ignore
  done;
  Dom.appendChild t.content contents
