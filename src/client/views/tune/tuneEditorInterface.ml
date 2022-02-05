open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  editor : TuneEditor.t;
  content : Html.divElement Js.t;
  input_name : Inputs.Text.t;
  input_kind : Inputs.Text.t;
  input_scddb_id : Inputs.Text.t;
  (* TODO: Need author too *)
}

let refresh t =
  Inputs.Text.set_contents t.input_name (TuneEditor.name t.editor);
  Inputs.Text.set_contents t.input_kind (TuneEditor.kind t.editor);
  Inputs.Text.set_contents t.input_scddb_id (TuneEditor.scddb_id t.editor)

let create ?on_save page =
  let editor = TuneEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Create a Tune") page in
  let form = Html.createForm (Page.document page) in
  let input_name = Inputs.Text.create
    ~placeholder:"Name of the tune"
    ~on_change:(fun name -> TuneEditor.set_name editor name)
    page
  in
  let input_kind = Inputs.Text.create
    ~placeholder:"Kind of the tune (J, P, R, S, W)"
    ~on_change:(fun kind -> TuneEditor.set_kind editor kind)
    page
  in
  let input_scddb_id = Inputs.Text.create
    ~placeholder:"Strathspey Database id (optional)"
    ~on_change:(fun id -> TuneEditor.set_scddb_id editor id)
    page
  in

  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");

  let t = {page; editor; content; input_name; input_kind; input_scddb_id} in

  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1, b2, b3 =
          Inputs.Text.check input_name (fun str -> str <> ""),
          Inputs.Text.check input_kind (fun str -> try Kind.base_of_string str |> ignore; true with _ -> false),
          Inputs.Text.check input_scddb_id (fun str -> str = "" || try int_of_string str >= 0 with _ -> false)
        in
        if b1 && b2 && b3 then (
          Lwt.on_success (TuneEditor.submit editor) (fun tune ->
          Lwt.on_success (Tune.slug tune) (fun slug ->
          begin match on_save with
          | None ->
            let href = Router.path_of_controller (Router.Tune slug) |> snd in
            Html.window##.location##.href := js href
          | Some cb -> cb slug
          end))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
        if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin
          TuneEditor.clear editor;
          Page.refresh page
        end)
      page
  in

  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);

  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_kind);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_scddb_id);
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
