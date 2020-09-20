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
    let h2 = Html.createH2 (Page.document page) in
    h2##.textContent := Js.some (js "Loading...");
    Lwt.on_success text (fun text -> h2##.textContent := Js.some (js text));
    {page; root = h2}

  let h3 ~text page =
    let h3 = Html.createH3 (Page.document page) in
    h3##.textContent := Js.some (js "Loading...");
    Lwt.on_success text (fun text -> h3##.textContent := Js.some (js text));
    {page; root = h3}

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

module Link = struct

  type root = Html.anchorElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let create ~href ~text page =
    let link = Html.createA (Page.document page) in
    Lwt.on_success text (fun text -> link##.textContent := Js.some (js text));
    Lwt.on_success href (fun href -> link##.href := js href);
    {page; root = link}

  let h1 ~href h1 =
    let page = h1.Heading.page in
    let link = Html.createA (Page.document page) in
    Lwt.on_success href (fun href -> link##.href := js href);
    Dom.appendChild link (Heading.root h1);
    {page; root = link}

  let root t =
    t.root

end
