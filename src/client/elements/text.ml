open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Heading = struct

  type root = Html.headingElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let h create ~text page =
    let h = create (Page.document page) in
    h##.textContent := Js.some (js "Loading...");
    Lwt.on_success text (fun text -> h##.textContent := Js.some (js text));
    {page; root = h}

  let h1 = h Html.createH1
  let h2 = h Html.createH2
  let h3 = h Html.createH3
  let h4 = h Html.createH4
  let h5 = h Html.createH5
  let h6 = h Html.createH6

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
