open Js_of_ocaml
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type root = Html.divElement

type 'a t = {
  page : Page.t;
  root : root Js.t;
  bar : Inputs.Text.t;
  table : Table.t;
  default : Table.Row.t option;
  empty : Table.Row.t;
  progress : Table.Row.t;
  search : string -> ('a Score.t) list Lwt.t;
  make_result : 'a Score.t -> Table.Row.t Lwt.t;
}

let root t = 
  t.root

let table t = 
  t.table

let bar t = 
  t.bar

let reset t = 
  Table.set_visible t.table false;
  let section = 
    match t.default with
    | None -> 
      Table.Section.create ~rows:(Lwt.return [t.progress]) t.page
    | Some d -> 
      Table.Section.create ~rows:(Lwt.return [t.progress; d]) t.page
  in
  Table.replace_bodies t.table (Lwt.return [section]);
  Inputs.Text.erase t.bar

let make_result_rows t =
  let make_row score = 
    let%lwt row = t.make_result score in
    Table.Row.on_click row (fun () -> reset t);
    Lwt.return row
  in
  let input = Inputs.Text.contents t.bar in
  if String.length input > 2 then begin
    let open Lwt in
    let results = t.search input in
    Lwt.bind results (fun scores ->
      if List.length scores > 0 then begin
        NesList.sub 10 scores
        |> Lwt_list.map_p make_row
        >>= (fun l -> 
          match t.default with
          | None -> Lwt.return (l)
          | Some d -> Lwt.return (l@[d]))
      end else begin
        match t.default with
        | None -> Lwt.return [t.empty]
        | Some d -> Lwt.return [t.empty; d]
      end)
  end else begin
    match t.default with
    | None -> Lwt.return [t.progress]
    | Some d -> Lwt.return [t.progress; d]
  end
   
let create ?default ~search ~placeholder ~make_result page =
  let root = Html.createDiv (Page.document page) in
  let table = Table.create ~visible:false ~kind:Table.Kind.Dropdown page in
  let bar = 
    Inputs.Text.create 
      ~default:placeholder
      ~on_focus:(fun b -> if b then Table.set_visible table true)
      page 
  in
  Dom.appendChild root (Inputs.Text.root bar);
  Dom.appendChild root (Table.root table);
  let empty =
    Table.Row.create
      ~cells:[
        Table.Cell.text ~text:(Lwt.return " ") page;
        Table.Cell.text ~text:(Lwt.return "No results") page]
      page
  in
  let progress =
    Table.Row.create
      ~cells:[
        Table.Cell.text ~text:(Lwt.return " ") page;
        Table.Cell.text ~text:(Lwt.return "Start typing to search") page]
      page
  in
  let t = {page; root; bar; table; default; empty; progress; search; make_result} in
  NesOption.ifsome 
    (fun default -> Table.Row.on_click default (fun () -> reset t))
    default;
  Inputs.Text.on_change bar (fun _ ->
    make_result_rows t
    |> (fun r -> Lwt.return [Table.Section.create ~rows:r t.page])
    |> Table.replace_bodies table);
  Page.register_modal page 
    ~element:(Table.root table :> Html.element Js.t)
    ~on_unfocus:(fun () -> Table.set_visible table false)
    ~targets:[(Table.root table :> Html.element Js.t); 
              (Inputs.Text.root bar :> Html.element Js.t)];
  reset t;
  t
