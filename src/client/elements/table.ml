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

  let link ~href ~text page = 
    let cell = Html.createTd (Page.document page) in
    let link = Html.createA (Page.document page) in
    Dom.appendChild cell link;
    link##.textContent := Js.some (js text);
    link##.href := js href;
    {page; root = cell}

  let text ~text page = 
    let cell = Html.createTd (Page.document page) in
    cell##.textContent := Js.some (js text);
    {page; root = cell}

  let root t = 
    t.root

end
    
module Row = struct

  type root = Html.tableRowElement

  type t = {
    page : Page.t;
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
    {page; root = row}

  let root t = 
    t.root

end

type root = Html.tableElement

type t = {
  page : Page.t;
  root : root Js.t;
  header : Html.tableRowElement Js.t;
}

let root t = 
  t.root

let add t tr = 
  Dom.appendChild t.root tr

let clear t = 
  while Js.to_bool t.root##hasChildNodes do
    let child = Js.Opt.get t.root##.firstChild (fun () -> assert false) in
    t.root##removeChild child |> ignore
  done;
  Dom.appendChild t.root t.header
