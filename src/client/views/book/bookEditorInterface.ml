open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    editor : BookEditor.t;
    content : Html.divElement Js.t;
    input_title : Inputs.Text.t;
  }

let refresh t =
  Inputs.Text.set_contents t.input_title (BookEditor.title t.editor)

let create ?on_save page =
  let editor = BookEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Add a Book") page in
  let form = Html.createForm (Page.document page) in
  let input_title = Inputs.Text.create
    ~placeholder:"Name of the book"
    ~on_change:(fun title -> BookEditor.set_title editor title)
    page
  in

  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");

  let t = {page; editor; content; input_title} in

  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1 =
          Inputs.Text.check input_title (fun str -> str <> "")

        in
        if b1 then (
          Lwt.on_success (BookEditor.submit editor) (fun book ->
            Lwt.on_success (Book.slug book) (fun slug ->
              begin match on_save with
                | None ->
                  let href = Router.path_of_controller (Router.Book slug) |> snd in
                  Html.window##.location##.href := js href
                | Some cb -> cb slug
              end))))
      page
  in

  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
        if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin
          BookEditor.clear editor;
          Page.refresh page;
          Inputs.Text.set_valid input_title true
        end)
      page
  in

  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);

  Dom.appendChild form (Inputs.Text.root input_title);

  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr (Page.document page));
  Dom.appendChild content (Html.createBr (Page.document page));
  Dom.appendChild content form;
  t

let contents t =
  t.content

let init t =
  refresh t
