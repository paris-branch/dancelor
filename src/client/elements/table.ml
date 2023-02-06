open Js_of_ocaml
open Js_of_ocaml_lwt

module Html = Dom_html

let js = Js.string

module Cell = struct

  type root = Html.tableCellElement

  type t = {
    page: Page.t;
    root: root Js.t;
  }

  let create ?width ?colspan ~content page =
    let cell = Html.createTd (Page.document page) in
    NesOption.ifsome (fun i -> cell##.colSpan := i) colspan;
    Style.set ?width cell;
    Lwt.on_success content @@ JsHelpers.add_children cell;
    { page; root = cell }

  let create_static ?width ?colspan ~content page =
    create ?width ?colspan ~content: (Lwt.return content) page

  let text ?width ?colspan ~text page =
    let text_node = (Page.document page)##createTextNode (js "") in
    Lwt.on_success text (fun text -> text_node##.data := js text);
    create ?width ?colspan ~content: (Lwt.return [text_node]) page

  let header_text ?width ?colspan ?alt ~text page =
    (* Cell, span & style *)
    let cell = Html.createTh (Page.document page) in
    NesOption.ifsome (fun i -> cell##.colSpan := i) colspan;
    Style.set ?width cell;
    (* Content *)
    begin
      match alt with
      | None ->
        let span = Html.createSpan (Page.document page) in
        Lwt.on_success text (fun text -> span##.textContent := Js.some (js text));
        Dom.appendChild cell span;
      | Some alt ->
        let span_main = Html.createSpan (Page.document page) in
        let span_alt = Html.createSpan (Page.document page) in
        Lwt.on_success text (fun text -> span_main##.textContent := Js.some (js text));
        Lwt.on_success alt (fun text -> span_alt##.textContent := Js.some (js text));
        span_main##.classList##add (js "full-content");
        span_alt##.classList##add (js "collapse-content");
        Dom.appendChild cell span_main;
        Dom.appendChild cell span_alt;
    end;
    { page; root = cell }

  let wrap_link ~href cell =
    let children = JsHelpers.extract_children cell.root in
    let link = Html.createA (Page.document cell.page) in
    link##.classList##add (js "fill");
    Lwt.on_success href (fun href -> link##.href := js href);
    JsHelpers.add_children link children;
    Dom.appendChild cell.root link

  let root t =
    t.root
end

module Row = struct

  type root = Html.tableRowElement

  type t = {
    page: Page.t;
    size: int;
    root: root Js.t;
  }

  let create ?on_click ?href ~cells page =
    ignore href;
    let row = Html.createTr (Page.document page) in
    List.iter (fun cell -> Cell.root cell |> Dom.appendChild row) cells;
    begin
      match href with
      | None -> ()
      | Some href ->
        List.iter (fun cell -> Cell.wrap_link ~href cell) cells;
        row##.classList##add (js "clickable");
      (* Lwt.async (fun () ->
           *   Lwt_js_events.clicks row
           *     (fun _ev _ ->
           *       Lwt.map (fun href -> Html.window##.location##.href := js href) href)) *)
    end;
    begin
      match on_click with
      | None -> ()
      | Some on_click ->
        row##.classList##add (js "clickable");
        Lwt.async
          (fun () ->
            Lwt_js_events.clicks
              row
              (fun _ev _ -> on_click (); Lwt.return ()))
    end;
    { page; size = List.length cells; root = row }

  let on_click row on_click =
    row.root##.classList##add (js "clickable");
    Lwt.async
      (fun () ->
        Lwt_js_events.clicks
          row.root
          (fun _ev _ -> on_click (); Lwt.return ()))

  let root t =
    t.root

  let size t =
    t.size
end

module Section = struct

  type root = Html.tableSectionElement

  type t = {
    page: Page.t;
    root: root Js.t;
    header: Row.t option;
  }

  let root t =
    t.root

  let add t tr =
    Dom.appendChild t.root (Row.root tr)

  let clear t =
    JsHelpers.clear_children t.root;
    NesOption.ifsome (fun h -> add t h) t.header

  let replace_rows t rows =
    Lwt.on_success
      rows
      (fun rows ->
        clear t;
        List.iter (add t) rows)

  let create ?header ~rows page =
    let root = Html.createTbody (Page.document page) in
    let t = { root; page; header } in
    replace_rows t rows;
    t

  let create_p ?header ~rows page =
    let root = Html.createTbody (Page.document page) in
    let t = { root; page; header } in
    replace_rows t rows;
    Lwt.bind
      rows
      (fun rows ->
        clear t;
        List.iter (add t) rows;
        Lwt.return t)
end

module Kind = struct

  type t =
    | Separated
    | Dropdown

  let to_class = function
    | Separated -> "separated-table"
    | Dropdown -> "dropdown-table"
end

type root = Html.tableElement

type t = {
  page: Page.t;
  root: root Js.t;
  head: Html.tableSectionElement Js.t;
}

let root t =
  t.root

let add t sec =
  Dom.appendChild t.root (Section.root sec)

let clear t =
  JsHelpers.clear_children t.root;
  Dom.appendChild t.root t.head

let replace_bodies t bodies =
  Lwt.on_success
    bodies
    (fun bodies ->
      clear t;
      List.iter (add t) bodies)

let hide t =
  t.root##.classList##remove (js "visible")

let show t =
  t.root##.classList##add (js "visible")

let set_visible t b =
  if b then show t
  else hide t

let create ?(visible = true) ?header ?contents ?kind page =
  let root = Html.createTable (Page.document page) in
  let head = Html.createThead (Page.document page) in
  let table = { page; root; head } in
  Dom.appendChild root head;
  NesOption.ifsome (fun h -> Dom.appendChild head (Row.root h)) header;
  NesOption.ifsome (fun k -> root##.classList##add (js (Kind.to_class k))) kind;
  NesOption.ifsome
    (fun contents ->
      let colspan =
        match header with
        | Some h -> Row.size h
        | None -> 1
      in
      let loading_row =
        Row.create ~cells: [Cell.text ~colspan ~text: (Lwt.return "Loading...") page] page
      in
      let loading = Section.create ~rows: (Lwt.return [loading_row]) page in
      add table loading;
      replace_bodies table contents)
    contents;
  set_visible table visible;
  table

let visible t =
  t.root##.classList##contains (js "visible")
  |> Js.to_bool
