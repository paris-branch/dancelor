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
  author_search : SearchBar.t;
  input_scddb_id : Inputs.Text.t;
}

let refresh t =
  Inputs.Text.set_contents t.input_name (TuneEditor.name t.editor);
  Inputs.Text.set_contents t.input_kind (TuneEditor.kind t.editor);
  begin match TuneEditor.author t.editor with
  | None -> Inputs.Text.set_contents (SearchBar.bar t.author_search) ""
  | Some cr ->
    let name = Credit.line cr in
    Lwt.on_success name (fun name ->
      Inputs.Text.set_contents (SearchBar.bar t.author_search) name)
  end;
  Inputs.Text.set_contents t.input_scddb_id (TuneEditor.scddb_id t.editor)

let make_author_modal editor content page =
  let modal_bg = Html.createDiv (Page.document page) in
  let credit_modal = Html.createDiv (Page.document page) in
  let interface =
    CreditEditorInterface.create page
      ~on_save:(fun slug ->
        Page.remove_modal page modal_bg;
        Dom.removeChild content modal_bg;
        Lwt.on_success (TuneEditor.set_author editor slug) (fun () -> Page.refresh page))
  in
  Dom.appendChild credit_modal (CreditEditorInterface.contents interface);
  credit_modal##.classList##add (js "modal-window");
  modal_bg##.classList##add (js "modal-background");
  Dom.appendChild modal_bg credit_modal;
  Dom.appendChild content modal_bg;
  Page.register_modal page
    ~element:modal_bg
    ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg)
    ~on_refresh:(fun () -> CreditEditorInterface.refresh interface)
    ~targets:[credit_modal]

let make_author_search_result editor page score =
  let author = Score.value score in
  let score = score.Score.score in
  let%lwt name = Credit.line author in
  let%lwt slug = Credit.slug author in
  let row = Table.Row.create
    ~on_click:(fun () ->
      Lwt.on_success
        (TuneEditor.set_author editor slug)
        (fun () -> Page.refresh page))
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
      Table.Cell.text ~text:(Lwt.return name) page]
    page
  in
  Lwt.return row

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
    ~placeholder:"Strathspey Database link or id (optional)"
    ~on_change:(fun id -> TuneEditor.set_scddb_id editor id)
    page
  in

  let author_search =
    let main_section =
      SearchBar.Section.create
        ~default:(Table.Row.create
          ~on_click:(fun () -> make_author_modal editor content page)
          ~cells:[
            Table.Cell.text ~text:(Lwt.return "  +") page;
            Table.Cell.text ~text:(Lwt.return "Create a new author") page]
          page)
        ~search:(fun input ->
            match CreditFilter.raw input with
            | Ok formula ->
              let%lwt results =
                Credit.search ~threshold:0.4
                  ~pagination:Pagination.{start = 0; end_ = 10} formula
              in
              Lwt.return_ok results
            | Error err -> Lwt.return_error err)
        ~make_result:(fun score -> make_author_search_result editor page score)
        page
    in
    SearchBar.create
      ~placeholder:"Author (Magic Search)"
      ~sections:[main_section]
      page
  in

  Inputs.Text.on_focus (SearchBar.bar author_search) (fun b ->
    if b then begin
      Inputs.Text.erase (SearchBar.bar author_search);
      TuneEditor.remove_author editor;
      Page.refresh page
    end);

  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");

  let t = {page; editor; content; input_name; input_kind; author_search; input_scddb_id} in

  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1, b2, b3 =
          Inputs.Text.check input_name (fun str -> str <> ""),
          Inputs.Text.check input_kind (fun str -> try Kind.base_of_string str |> ignore; true with _ -> false),
          Inputs.Text.check input_scddb_id (fun str ->
              if str = "" then
                true
              else
                match int_of_string_opt str with
                | Some _ -> true
                | None ->
                  match SCDDB.tune_from_uri str with
                  | Ok _ -> true
                  | Error _ -> false
            )
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
  Dom.appendChild form (SearchBar.root author_search);
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
