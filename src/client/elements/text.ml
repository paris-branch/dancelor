open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Heading = struct

  type root = Html.headingElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let h1 ~text page = 
    let h1 = Html.createH1 (Page.document page) in
    h1##.textContent := Js.some (js "Loading...");
    Lwt.on_success text (fun text -> h1##.textContent := Js.some (js text));
    {page; root = h1}

  let h2 ~text page = 
    let h2 = Html.createH1 (Page.document page) in
    h2##.textContent := Js.some (js "Loading...");
    Lwt.on_success text (fun text -> h2##.textContent := Js.some (js text));
    {page; root = h2}

  let root t = 
    t.root

end

module Paragraph = struct

  type root = Html.paragraphElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let create ?placeholder ~text page = 
    let p = Html.createP (Page.document page) in
    NesOption.ifsome (fun ph -> p##.textContent := Js.some (js ph)) placeholder;
    Lwt.on_success text (fun text -> p##.textContent := Js.some (js text));
    {page; root = p}

  let root t = 
    t.root

end
