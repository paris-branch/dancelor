open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_client_utils

module Html = Dom_html

let js = Js.string

type t = {
  page: Page.t;
  editor: BookEditor.t;
  content: Html.divElement Js.t;
  input_title: Inputs.Text.t;
  input_date: Inputs.Text.t;
  sets_area: Html.divElement Js.t;
  sets_search: SearchBar.t;
}

let make_set_subwindow t index set =
  let subwin = Html.createDiv (Page.document t.page) in
  subwin##.classList##add (js "subwindow");
  let toolbar = Html.createDiv (Page.document t.page) in
  toolbar##.classList##add (js "toolbar");
  let title = Text.Heading.h3_static ~text: (Set.name (snd set)) t.page in
  Dom.appendChild toolbar (Text.Heading.root title);
  let buttons = Html.createUl (Page.document t.page) in
  let down, up, del =
    Inputs.Button.create
      ~on_click: (fun () ->
        BookEditor.move_down t.editor index;
        Page.refresh t.page)
      ~icon: "chevron-down"
      t.page,
    Inputs.Button.create
      ~on_click: (fun () ->
        BookEditor.move_up t.editor index;
        Page.refresh t.page)
      ~icon: "chevron-up"
      t.page,
    Inputs.Button.create
      ~on_click: (fun () ->
        BookEditor.remove t.editor index;
        Page.refresh t.page)
      ~kind: Inputs.Button.Kind.Danger
      ~icon: "times"
      t.page
  in
  let downli, upli, delli =
    Html.createLi (Page.document t.page),
    Html.createLi (Page.document t.page),
    Html.createLi (Page.document t.page)
  in
  Dom.appendChild downli (Inputs.Button.root down);
  Dom.appendChild upli (Inputs.Button.root up);
  Dom.appendChild delli (Inputs.Button.root del);
  Dom.appendChild buttons downli;
  Dom.appendChild buttons upli;
  Dom.appendChild buttons delli;
  Dom.appendChild toolbar buttons;
  Dom.appendChild subwin toolbar;
  subwin

let refresh t =
  Inputs.Text.set_contents t.input_title (BookEditor.title t.editor);
  Inputs.Text.set_contents t.input_date (BookEditor.date t.editor);
  Helpers.clear_children t.sets_area;
  BookEditor.iter
    t.editor
    (fun i set ->
      let subwin = make_set_subwindow t i set in
      Dom.appendChild t.sets_area (Html.createBr (Page.document t.page));
      Dom.appendChild t.sets_area subwin)

let make_set_search_result editor page score =
  let set = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Set.slug set in
  let%lwt name = Set.name set in
  let row =
    Table.Row.create
      ~on_click: (fun () ->
        Lwt.on_success (BookEditor.add editor slug) (fun () -> Page.refresh page))
      ~cells: [
        Table.Cell.text ~text: (Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
        Table.Cell.text ~text: (Lwt.return name) page;
      ]
      page
  in
  Lwt.return row

let create ?on_save page =
  let editor = BookEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text: (Lwt.return "Add a Book") page in
  let form = Html.createForm (Page.document page) in
  let input_title =
    Inputs.Text.create
      ~placeholder: "Name of the book"
      ~on_change: (fun title -> BookEditor.set_title editor title)
      page
  in
  let input_date =
    Inputs.Text.create
      ~placeholder: "Date for the book, in the YYYY-MM-DD format (optional)"
      ~on_change: (fun date -> BookEditor.set_date editor date)
      page
  in
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display: "flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let sets_area = Html.createDiv (Page.document page) in
  let sets_search =
    let main_section =
      SearchBar.Section.create
        ~search: (fun input ->
          let%rlwt formula = Lwt.return (SetFilter.raw input) in
          let%lwt results =
            Set.search
              ~threshold: 0.4
              ~pagination: Pagination.{ start = 0; end_ = 10 }
              formula
          in
          Lwt.return_ok results)
        ~make_result: (fun score -> make_set_search_result editor page score)
        page
    in
    SearchBar.create
      ~placeholder: "Add set (Magic Search)"
      ~sections: [main_section]
      page
  in
  let t = { page; editor; content; input_title; input_date; sets_area; sets_search } in
  let save =
    Inputs.Button.create
      ~kind: Inputs.Button.Kind.Success
      ~icon: "save"
      ~text: "Save"
      ~on_click: (fun () ->
        let b1, b2 =
          Inputs.Text.check input_title (fun str -> str <> ""),
          Inputs.Text.check
            input_date
            (fun str -> try PartialDate.from_string str |> ignore; true with _ -> str = "")
        in
        if b1 && b2 then
          (Lwt.on_success
            (BookEditor.submit editor)
            (fun book ->
              Lwt.on_success
                (Book.slug book)
                (fun slug ->
                  begin
                    match on_save with
                    | None ->
                      let href = Router.path_of_controller (Router.Book slug) |> snd in
                      Html.window##.location##.href := js href
                    | Some cb -> cb slug
                  end))))
      page
  in
  let clear =
    Inputs.Button.create
      ~kind: Inputs.Button.Kind.Danger
      ~icon: "exclamation-triangle"
      ~text: "Clear"
      ~on_click: (fun () ->
        if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then
          begin
            BookEditor.clear editor;
            Page.refresh page;
            Inputs.Text.set_valid input_title true;
            Inputs.Text.set_valid input_date true
          end)
      page
  in
  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);
  Dom.appendChild form (Inputs.Text.root input_title);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_date);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form sets_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root sets_search);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr (Page.document page));
  Dom.appendChild content (Html.createBr (Page.document page));
  Dom.appendChild content form;
  t

let contents t =
  t.content

let init t =
  refresh t
