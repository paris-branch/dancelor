open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type t = {
  document: Html.document Js.t;
  content: Html.divElement Js.t;
}

let create _ =
  let document = Html.window##.document in
  let content = Html.createDiv document in
  let text = Html.createEm document in
  text##.textContent := Js.some (js "C'est spartiate parce que c'est une URL invalide ;-)");
  Dom.appendChild content text;
  { document; content }

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
