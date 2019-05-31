open Js_of_ocaml

module Html = Dom_html
let js = Js.string

let i cls page =
  let i = Html.createI (Page.document page) in
  i##.classList##add (js "fas");
  i##.classList##add (js ("fa-" ^ cls));
  i
