open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    editor : CreditEditor.t;
    content : Html.divElement Js.t;
    input_name : Inputs.Text.t;
    input_scddb_id : Inputs.Text.t;
  }

let refresh t =
  Inputs.Text.set_contents t.input_name (CreditEditor.name t.editor);
  Inputs.Text.set_contents t.input_scddb_id (CreditEditor.scddb_id t.editor)

let create ?on_save page =
  let editor = CreditEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Create a Credit") page in
  let form = Html.createForm (Page.document page) in
  let input_name = Inputs.Text.create
      ~placeholder:"Display name"
      ~on_change:(fun name -> CreditEditor.set_name editor name)
      page
  in
  let input_scddb_id = Inputs.Text.create
      ~placeholder:"Strathspey Database link or id (optional)"
      ~on_change:(fun id -> CreditEditor.set_scddb_id editor id)
      page
  in
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let t = {page; editor; content; input_name; input_scddb_id} in
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
          let b1, b2 =
            Inputs.Text.check input_name (fun str -> str <> ""),
            Inputs.Text.check input_scddb_id (fun str ->
                if str = "" then
                  true
                else
                  match int_of_string_opt str with
                  | Some _ -> true
                  | None ->
                    match SCDDB.person_from_uri str with
                    | Ok _ -> true
                    | Error _ -> false
              )
          in
          if b1 && b2 then (
            Lwt.on_success (CreditEditor.submit editor) (fun credit ->
                Lwt.on_success (Credit.slug credit) (fun slug ->
                    begin match on_save with
                      | None -> Html.window##.location##.href := js PageRouter.(path (Credit slug))
                      | Some cb -> cb slug
                    end))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
          if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin
            CreditEditor.clear editor;
            Page.refresh page;
            Inputs.Text.set_valid input_name true;
            Inputs.Text.set_valid input_scddb_id true
          end)
      page
  in
  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr (Page.document page));
  Dom.appendChild content (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_scddb_id);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content form;
  t

let contents t =
  t.content

let init t =
  refresh t
