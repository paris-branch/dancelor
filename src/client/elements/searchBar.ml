open Nes
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

module Section = struct

  type 'a t = {
    page : Page.t;
    search : string -> ('a list, string) result Lwt.t;
    section : Table.Section.t;
    default : Table.Row.t option;
    empty : Table.Row.t;
    make_result : 'a -> Table.Row.t Lwt.t;
    header : Table.Row.t option;
  }

  type wrapped = Wrapped : _ t -> wrapped (* GADT cuz why not *)

  let create ~search ~make_result ?header ?default page =
    let section = Table.Section.create ?header ~rows:(Lwt.return []) page in
    let empty =
      Table.Row.create
        ~cells:[
          Table.Cell.text ~text:(Lwt.return "⚠️") page;
          Table.Cell.text ~text:(Lwt.return "Your search returned no results.") page]
        page
    in
    let t = {page; search; section; default; empty; make_result; header} in
    Wrapped t

  let body (Wrapped t) =
    t.section

  let reset (Wrapped t) =
    Table.Section.clear t.section

  let make_error_row (Wrapped t) message =
    let open Table in
    Lwt.return (
      Row.create
        ~cells:[
          Cell.text ~text:(Lwt.return "❌") t.page;
          Cell.text ~text:(Lwt.return message) t.page
        ]
        t.page
    )

  let make_result_rows (Wrapped t) input cb =
    let make_row score =
      let%lwt row = t.make_result score in
      Table.Row.on_click row (fun () -> cb ());
      Lwt.return row
    in
    if String.length input > 2 then
      (
        match%lwt t.search input with
        | Ok scores ->
          (
            if List.length scores > 0 then
              NesList.sub 10 scores
              |> Lwt_list.map_p make_row
              >>=| fun l ->
              match t.default with
              | None -> Lwt.return l
              | Some d -> Lwt.return (l @ [d])
            else
              match t.default with
              | None -> Lwt.return [t.empty]
              | Some d -> Lwt.return [t.empty; d]
          )
        | Error error ->
          Lwt.map List.singleton (make_error_row (Wrapped t) error)
      )
    else
      match t.default with
      | None -> Lwt.return []
      | Some d -> Lwt.return [d]

  let update t input cb =
    let Wrapped wt = t in
    make_result_rows t input cb
    |> Table.Section.replace_rows wt.section
end

type root = Html.divElement

type t = {
  page : Page.t;
  root : root Js.t;
  bar : Inputs.Text.t;
  table : Table.t;
  messages : Table.Section.t;
  sections : Section.wrapped list;
  hide_sections : bool;
}

let root t =
  t.root

let table t =
  t.table

let bar t =
  t.bar

let rec reset t =
  Table.set_visible t.table false;
  if t.hide_sections then
    Table.replace_bodies t.table (Lwt.return [t.messages])
  else begin
    let bodies = List.map Section.body t.sections in
    Table.replace_bodies t.table (Lwt.return (t.messages :: bodies));
  end;
  List.iter Section.reset t.sections;
  Inputs.Text.erase t.bar;
  update t

and update t =
  let input = Inputs.Text.contents t.bar in
  List.iter (fun s -> Section.update s input (fun () -> reset t)) t.sections;
  if String.length input < 3 then
    (
      let info_message =
        if input = "" then "Start typing to search."
        else "Type at least three characters."
      in
      let messages =
        Table.Row.create
          ~cells:[
            Table.Cell.text ~icon:"keyboard" ~colspan:0 ~text:(Lwt.return info_message) t.page
          ]
          t.page
      in
      Table.Section.replace_rows t.messages (Lwt.return [messages]);
      if t.hide_sections then
        Table.replace_bodies t.table (Lwt.return [t.messages])
    )
  else
    (
      let bodies = List.map Section.body t.sections in
      let enter_for_more =
        Table.Row.create
          ~cells:[
            Table.Cell.text ~icon:"info" ~colspan:0 ~text:(Lwt.return "Press enter for more results.") t.page
          ]
          t.page
      in
      Table.Section.replace_rows t.messages (Lwt.return [enter_for_more]);
      Table.replace_bodies t.table (Lwt.return (bodies @ [t.messages]))
    )

let create ~placeholder ~sections ?on_enter ?(hide_sections = false) page =
  let root = Html.createDiv (Page.document page) in
  let table = Table.create ~visible:false ~kind:Table.Kind.Dropdown page in
  let bar =
    Inputs.Text.create
      ~placeholder
      ~on_focus:(fun b -> if b then Table.set_visible table true)
      page
  in
  let messages = Table.Section.create ~rows:(Lwt.return []) page in
  Dom.appendChild root (Inputs.Text.root bar);
  Dom.appendChild root (Table.root table);
  let t = {page; root; bar; table; messages; sections; hide_sections} in
  List.iter (fun (Section.Wrapped section) ->
      Option.iter
        (fun default -> Table.Row.on_click default (fun () -> reset t))
        section.Section.default) sections;
  Inputs.Text.on_change bar (fun _ -> update t);
  Option.iter (fun on_enter ->
      Inputs.Text.on_enter bar on_enter)
    on_enter;
  Page.register_modal page
    ~element:(Table.root table :> Html.element Js.t)
    ~on_unfocus:(fun () -> Table.set_visible table false)
    ~targets:[(Table.root table :> Html.element Js.t);
              (Inputs.Text.root bar :> Html.element Js.t)];
  reset t;
  t

let focus t =
  (Inputs.Text.root t.bar)##focus
