open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type root = Html.divElement

type t = {
  page: Page.t;
  root: root Js.t;
}

let create ~source page =
  let root = Html.createDiv (Page.document page) in
  root##.classList##add (js "image-container");
  root##.textContent := Js.some (js "Loading image...");
  Lwt.on_success
    source
    (fun src ->
      let img = Html.createImg (Page.document page) in
      img##.src := js src;
      root##.textContent := Js.null;
      Dom.appendChild root img);
  { page; root }

let root t =
  t.root
