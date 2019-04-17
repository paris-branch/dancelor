open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type t = 
{
  document : Html.document Js.t;
  content : Html.divElement Js.t;
  menu : Html.uListElement Js.t;
}

let create () = 
  let document = Html.window##.document in
  let content = Html.createDiv document in
  let title = Html.createH1 document in
  let menu = Html.createUl document in
  title##.textContent := Js.some (js "Dancelor");
  menu##.id := js "menu";
  Dom.appendChild content title;
  Dom.appendChild content menu;
  {document; content; menu}

let contents t =
  t.content

let add_menu_entry t name target = 
  let entry = Html.createLi t.document in
  let link = Html.createA t.document in
  Dom.appendChild entry link;
  link##.textContent := Js.some (js name);
  link##.href := js target;
  Dom.appendChild t.menu entry
