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
    link##.href := js href;
    {page; root = cell}

  let text ?width ?span ~text page = 
    let cell = Html.createTd (Page.document page) in
    NesOption.ifsome (fun i -> cell##.colSpan := i) span;
    Style.set ?width cell;
    Lwt.on_success text (fun text -> cell##.textContent := Js.some (js text));
    {page; root = cell}

  let header_text ?width ?span ~text page = 
    let cell = Html.createTh (Page.document page) in
    NesOption.ifsome (fun i -> cell##.colSpan := i) span;
    Style.set ?width cell;
    Lwt.on_success text (fun text -> cell##.textContent := Js.some (js text));
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
      Lwt.async (fun () ->
        Lwt_js_events.clicks row
          (fun _ev _ ->
            Html.window##.location##.href := js href;
            Lwt.return ()))
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
  header : Row.t;
}

let root t = 
  t.root

let add t tr = 
  Dom.appendChild t.root (Row.root tr)

let clear t = 
  while Js.to_bool t.root##hasChildNodes do
    let child = Js.Opt.get t.root##.firstChild (fun () -> assert false) in
    t.root##removeChild child |> ignore
  done;
  add t t.header

let create ~header ~contents page =
  let root = Html.createTable (Page.document page) in
  let table = {page; root; header} in
  let loading = 
    Row.create 
      ~cells:[
        Cell.text ~span:(Row.size header) ~text:(Lwt.return "Loading...") page] 
      page 
  in
  add table header;
  add table loading;
  root##.classList##add (js "separated-table");
  (Row.root header)##.classList##add (js "table-header");
  Lwt.on_success contents (fun contents ->
    clear table;
    List.iter (add table) contents);
  table
