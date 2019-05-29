open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let set_style ?width ?color ?margin ?display (decl : Html.cssStyleDeclaration Js.t) = 
  NesOption.ifsome (fun w -> decl##.width := js w) width;
  NesOption.ifsome (fun c -> decl##.backgroundColor := js c) color;
  NesOption.ifsome (fun m -> decl##.margin := js m) margin;
  NesOption.ifsome (fun d -> decl##.display := js d) display

let set ?width ?color ?margin ?display elt = 
  set_style ?width ?color ?margin ?display elt##.style

let display elt = 
  Js.to_string elt##.style##.display
