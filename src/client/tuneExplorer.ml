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
  let text = Html.createEm document in
  text##.textContent := Js.some (js "Tu ne le vois peut-être pas, mais ceci n'est pas un texte. C'est un magnifique explorateur de tunes, le plus splendide jamais conçu.");
  Dom.appendChild content text;
  {document; content}

let contents t =
  t.content
