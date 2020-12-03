open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Heading = struct

  type root = Html.headingElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let h create ~content page =
    let h = create (Page.document page) in
    h##.textContent := Js.some (js "");
    Lwt.on_success content @@ JsHelpers.add_children h;
    {page; root = h}

  let h1 ~content page = h Html.createH1 ~content page
  let h2 ~content page = h Html.createH2 ~content page
  let h3 ~content page = h Html.createH3 ~content page
  let h4 ~content page = h Html.createH4 ~content page
  let h5 ~content page = h Html.createH5 ~content page
  let h6 ~content page = h Html.createH6 ~content page

  let h_static create ~text page =
    let h = create (Page.document page) in
    h##.textContent := Js.some (js "");
    Lwt.on_success text (fun text -> h##.textContent := Js.some (js text));
    {page; root = h}

  let h1_static = h_static Html.createH1
  let h2_static = h_static Html.createH2
  let h3_static = h_static Html.createH3
  let h4_static = h_static Html.createH4
  let h5_static = h_static Html.createH5
  let h6_static = h_static Html.createH6

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
