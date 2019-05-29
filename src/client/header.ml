open Dancelor_client_elements
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  content : Html.divElement Js.t;
  menu : Html.uListElement Js.t;
}

let create page =
  let document = Page.document page in
  let content = Html.createDiv document in
  content##.classList##add (js "content");
  let menu_toggle =
    let toggle = Html.createA document in
    toggle##.id := js "to_nav";
    let icon = Html.createI document in
    icon##.classList##add (js "fas");
    icon##.classList##add (js "fa-bars");
    Dom.appendChild toggle icon;
    toggle
  in
  (* FIXME: onclick: toggleMenu(); *)
  let title = Html.createH1 document in
  title##.textContent := Js.some (js "Dancelor");
  let menu = Html.createUl document in
  menu##.id := js "nav";
  Dom.appendChild content menu_toggle;
  Dom.appendChild content title;
  Dom.appendChild content menu;
  {page; content; menu}

let contents t =
  t.content

let add_menu_entry t name target =
  let document = Page.document t.page in
  let entry = Html.createLi document in
  let link = Html.createA document in
  Dom.appendChild entry link;
  link##.textContent := Js.some (js name);
  link##.href := js target;
  Dom.appendChild t.menu entry
