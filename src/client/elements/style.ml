open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let set_style ?width decl = 
  NesOption.ifsome (fun w -> decl##.width := js w) width

let set ?width elt = 
  set_style ?width elt##.style
