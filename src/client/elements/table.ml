open Js_of_ocaml
open Js_of_ocaml_lwt

module Html = Dom_html

let js = Js.string

module Cell = struct

  type root = Html.tableCellElement

  type t = {
    page : Page.t;
    root : root Js.t;
  }

  let link ?width ?span ~href ~text page = 
    let cell = Html.createTd (Page.document page) in
    let link = Html.createA (Page.document page) in
    Dom.appendChild cell link;
    NesOption.ifsome (fun i -> cell##.colSpan := i) span;
    Style.set ?width cell;
    Lwt.on_success text (fun text -> link##.textContent := Js.some (js text));
    Lwt.on_success href (fun href -> link##.href := js href);
    {page; root = cell}

  let text ?width ?span ~text page = 
    let cell = Html.createTd (Page.document page) in
    NesOption.ifsome (fun i -> cell##.colSpan := i) span;
    Style.set ?width cell;
    Lwt.on_success text (fun text -> cell##.textContent := Js.some (js text));
    {page; root = cell}

  let header_text ?width ?span ?alt ~text page = 
    let cell = Html.createTh (Page.document page) in
    NesOption.ifsome (fun i -> cell##.colSpan := i) span;
    Style.set ?width cell;
    begin match alt with
    | None -> 
      Lwt.on_success text (fun text -> cell##.textContent := Js.some (js text))
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
    {page; root = cell}

  let root t = 
    t.root

end
    
module Row = struct

  type root = Html.tableRowElement

  type t = {
    page : Page.t;
    size : int;
    root : root Js.t;
  }

  let create ?href ~cells page = 
    ignore href;
    let row = Html.createTr (Page.document page) in
    List.iter (fun cell -> Cell.root cell |> Dom.appendChild row) cells;
    begin match href with
    | None -> ()
    | Some href -> 
      row##.classList##add (js "clickable");
      Lwt.async (fun () ->
        Lwt_js_events.clicks row
          (fun _ev _ ->
            Lwt.map (fun href -> Html.window##.location##.href := js href) href))
    end;
    {page; size = List.length cells; root = row}

  let root t = 
    t.root

  let size t = 
    t.size

end

type root = Html.tableElement

type t = {
  page : Page.t;
  root : root Js.t;
  head : Html.tableSectionElement Js.t;
  body : Html.tableSectionElement Js.t;
}

let root t = 
  t.root

let add t tr = 
  Dom.appendChild t.body (Row.root tr)

let clear t = 
  while Js.to_bool t.body##hasChildNodes do
    let child = Js.Opt.get t.body##.firstChild (fun () -> assert false) in
    t.body##removeChild child |> ignore
  done

let create ~header ~contents page =
  let root = Html.createTable (Page.document page) in
  let head = Html.createThead (Page.document page) in
  let body = Html.createTbody (Page.document page) in
  let table = {page; root; head; body} in
  Dom.appendChild root head;
  Dom.appendChild root body;
  Dom.appendChild head (Row.root header);
  let loading = 
    Row.create 
      ~cells:[
        Cell.text ~span:(Row.size header) ~text:(Lwt.return "Loading...") page] 
      page 
  in
  add table loading;
  root##.classList##add (js "separated-table");
  Lwt.on_success contents (fun contents ->
    clear table;
    List.iter (add table) contents);
  table
