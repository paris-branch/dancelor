open Js_of_ocaml
open Dancelor_client_model
open Dancelor_client_elements
open Dancelor_common

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    editor : VersionEditor.t;
    content : Dom_html.divElement Js.t;
    input_name : Inputs.Text.t;
  }

let refresh t =
  Inputs.Text.set_contents t.input_name (VersionEditor.name t.editor)

let create page =
  let document = Page.document page in
  document##.title := js "Add a Tune | Dancelor";

  let editor = VersionEditor.create () in
  let content = Dom_html.createDiv document in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Add a new tune") page in
  let form = Html.createForm document in
  let input_name = Inputs.Text.create
    ~placeholder:"Name"
    (* TODO: Do we need a local storage similar to setCompose? *)
    ~on_change:(fun name -> VersionEditor.set_name editor name)
    page
  in

  let t =
    {page; editor; content; input_name}
  in

  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1 =
          Inputs.Text.check t.input_name (fun str -> str <> "")
        in
        if b1 then (
          Lwt.on_success (VersionEditor.submit editor) (fun version ->
          Lwt.on_success (Version.slug version) (fun slug ->
          let href = Router.path_of_controller (Router.Version slug) |> snd in
          Html.window##.location##.href := js href))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
    ~on_click:(fun () ->
      if Html.window##confirm (js "Clear the version?") |> Js.to_bool then begin
        VersionEditor.clear editor;
        refresh t
      end)
    page
  in

  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);

  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form submit;

  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content form;

  t


let contents t =
  t.content

let init t =
  refresh t
