open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js

let to_html str =
  let html_str = Omd.to_html (Omd.of_string str) in
  let div = Dom_html.document##createElement (Js.string "div") in
  div##.innerHTML := Js.string html_str;
  Of_dom.of_div div
