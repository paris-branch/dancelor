open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let set_style ?width ?color ?margin (decl : Html.cssStyleDeclaration Js.t) = 
  NesOption.ifsome (fun w -> decl##.width := js w) width;
  NesOption.ifsome (fun c -> decl##.backgroundColor := js c) color;
  NesOption.ifsome (fun m -> decl##.margin := js m) margin

let set ?width ?color ?margin elt = 
  set_style ?width ?color ?margin elt##.style
