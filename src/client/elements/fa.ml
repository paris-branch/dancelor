open Js_of_ocaml

module Html = Dom_html
let js = Js.string

let i cls page =
  let i = Html.createI (Page.document page) in
  let text = (Page.document page)##createTextNode (js cls) in
  Dom.appendChild i text;
  i##.classList##add (js "material-symbols-outlined");
  i
