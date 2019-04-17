open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type t = 
{
  document : Html.document Js.t;
  content : Html.divElement Js.t;
}

let create () = 
  let document = Html.window##.document in
  let content = Html.createDiv document in
  content##.textContent := Js.some (js "Dancelor");
  {document; content}

let contents t =
  t.content
