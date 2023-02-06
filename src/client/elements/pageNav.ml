open Nes
open Js_of_ocaml
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type root = Html.divElement

type t = {
  page: Page.t;
  mutable max_pages: int;
  mutable entries: int;
  entries_per_page: int;
  root: root Js.t;
  nav: Html.uListElement Js.t;
  info: Html.divElement Js.t;
  mutable cur_page: int;
  mutable on_page_change: int -> unit;
}

let set_current ?(force = false) t p =
  if (p <> t.cur_page || force) && p <= t.max_pages && p >= 1 then
    begin
      t.cur_page <- p;
      t.on_page_change p
    end

let make_button ?(disabled = false) ?(active = false) ?(dest = 0) t text =
  let li = Html.createLi (Page.document t.page) in
  if active then
    li##.classList##add (js "active");
  let on_click =
    if (not disabled) && (not active) && (dest <> 0) then
      Some (fun () -> set_current t dest)
    else
      None
  in
  let button = Inputs.Button.create ~text ?on_click t.page in
  Inputs.Button.set_enabled button (not disabled);
  Dom.appendChild li (Inputs.Button.root button);
  li

let make_ellipsis t =
  make_button ~disabled: true t "..."

let make_button_list t pages =
  let first, prev, next, last =
    make_button ~dest: 1 ~disabled: (t.cur_page = 1) t "First",
    make_button ~dest: (t.cur_page - 1) ~disabled: (t.cur_page = 1) t "Previous",
    make_button ~dest: (t.cur_page + 1) ~disabled: (t.cur_page = t.max_pages) t "Next",
    make_button ~dest: t.max_pages ~disabled: (t.cur_page = t.max_pages) t "Last"
  in
  let rec aux prev = function
    | [] -> [next; last]
    | h :: tl ->
      let tl_nav = (make_button ~dest: h ~active: (h = t.cur_page) t (string_of_int h)) :: (aux h tl) in
      if h <> prev + 1 then
        (make_ellipsis t) :: tl_nav
      else
        tl_nav
  in
  first :: prev :: aux 0 pages

let build_page_list cur_page max_pages =
  List.init max_pages (fun i -> i + 1)
  |> List.filter
    (fun i ->
      (i = 1)
      || (i = max_pages)
      || (abs (cur_page - i) <= 1)
      || (i <= 5 && cur_page <= 4)
      || (i > max_pages - 5 && cur_page > max_pages - 4))

let pagination t =
  let f, l =
    ((t.cur_page - 1) * t.entries_per_page),
    (min (t.cur_page * t.entries_per_page) t.entries)
  in
  Pagination.{ start = f; end_ = l }

let make_info t =
  if t.cur_page = 0 then
    spf "Loading entries..."
  else
    if t.entries = 0 then
      spf "No entries"
    else
      begin
        let pag = pagination t in
        spf "Showing %i to %i of %i entries" (Pagination.start pag + 1) (Pagination.end_ pag) t.entries
      end

let rebuild t =
  JsHelpers.clear_children t.nav;
  build_page_list t.cur_page t.max_pages
  |> make_button_list t
  |> List.iter (Dom.appendChild t.nav);
  t.info##.textContent := Js.some (js (make_info t))

let current t =
  t.cur_page

let create ~entries ~entries_per_page page =
  let root = Html.createDiv (Page.document page) in
  let nav = Html.createUl (Page.document page) in
  let info = Html.createDiv (Page.document page) in
  root##.id := js "page_nav";
  Dom.appendChild root info;
  Dom.appendChild root nav;
  let on_page_change = fun _ -> () in
  let max_pages = (entries + (entries_per_page - 1)) / entries_per_page in
  let t = { page; entries; max_pages; entries_per_page; root; nav; info; cur_page = 0; on_page_change } in
  rebuild t;
  t

let set_entries t entries =
  t.entries <- entries;
  let max_pages = (entries + (t.entries_per_page - 1)) / t.entries_per_page in
  t.max_pages <- max max_pages 1;
  set_current ~force: true t 1

let connect_on_page_change t f =
  t.on_page_change <- f

let root t =
  t.root
