open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let set_style ?width ?color ?margin ?display (decl : Html.cssStyleDeclaration Js.t) =
  Option.iter (fun w -> decl##.width := js w) width;
  Option.iter (fun c -> decl##.backgroundColor := js c) color;
  Option.iter (fun m -> decl##.margin := js m) margin;
  Option.iter (fun d -> decl##.display := js d) display

let set ?width ?color ?margin ?display elt =
  set_style ?width ?color ?margin ?display elt##.style

let display elt =
  Js.to_string elt##.style##.display
