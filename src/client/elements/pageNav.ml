open Js_of_ocaml

module Html = Dom_html

let js = Js.string

type root = Html.uListElement

type t = {
  page : Page.t;
  max_pages : int;
  root : root Js.t;
  mutable cur_page : int;
  on_page_change : int -> unit;
}

let make_button t nb = 
  let li = Html.createLi (Page.document t.page) in
  if nb = t.cur_page then
    li##.classList##add (js "active");
  let button = Inputs.Button.create ~text:(string_of_int nb) t.page in
  Dom.appendChild li (Inputs.Button.root button);
  li

let make_ellipsis t = 
  let li = Html.createLi (Page.document t.page) in
  li##.textContent := Js.some (js "...");
  li

let make_exhaustive_nav t = 
  List.init t.max_pages (fun i -> make_button t (i+1))

let rebuild t =
  JsHelpers.clear_children t.root;
  let buttons =
    if t.max_pages <= 6 then
      make_exhaustive_nav t
    else begin
      let cur = make_button t t.cur_page in
      let head = 
        if t.cur_page > 3 then [make_button t 1; make_ellipsis t]
        else if t.cur_page = 3 then [make_button t 1; make_button t 2]
        else if t.cur_page = 2 then [make_button t 1]
        else []
      in
      let tail = 
        if t.max_pages - t.cur_page > 2 then
          [make_ellipsis t; make_button t t.max_pages]
        else if t.max_pages - t.cur_page = 2 then 
          [make_button t (t.max_pages - 1); make_button t (t.max_pages)]
        else if t.max_pages - t.cur_page = 1 then 
          [make_button t (t.max_pages)]
        else []
      in
      head @ [cur] @ tail
    end
  in
  List.iter (Dom.appendChild t.root) buttons

let current t = 
  t.cur_page

let set_current t p = 
  if p <> t.cur_page && p <= t.max_pages && p >= 1 then begin
    t.cur_page <- p;
    t.on_page_change p
  end

let create ~max_pages ~on_page_change page =
  let root = Html.createUl (Page.document page) in
  let nav = {page; max_pages; root; cur_page = 1; on_page_change} in
  rebuild nav;
  nav

let root t = 
  t.root
