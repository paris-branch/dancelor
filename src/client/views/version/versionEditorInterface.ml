open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_elements

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    editor : VersionEditor.t;
    content : Dom_html.divElement Js.t;
    tune_search : SearchBar.t;
    input_bars : Inputs.Text.t;
    input_key : Inputs.Text.t;
    input_structure : Inputs.Text.t;
    arranger_search : SearchBar.t;
    input_remark : Inputs.Text.t;
    input_disambiguation : Inputs.Text.t;
    input_content : Inputs.Textarea.t;
  }

let refresh t =
  begin match VersionEditor.tune t.editor with
    | None -> Inputs.Text.set_contents (SearchBar.bar t.tune_search) ""
    | Some tune ->
      let name = Tune.name tune in
      Lwt.on_success name (fun name ->
          Inputs.Text.set_contents (SearchBar.bar t.tune_search) name)
  end;
  begin match VersionEditor.arranger t.editor with
    | None -> Inputs.Text.set_contents (SearchBar.bar t.arranger_search) ""
    | Some arranger ->
      let name = Person.line arranger in
      Lwt.on_success name (fun name ->
          Inputs.Text.set_contents (SearchBar.bar t.arranger_search) name)
  end;
  Inputs.Text.set_contents t.input_bars (VersionEditor.bars t.editor);
  Inputs.Text.set_contents t.input_key (VersionEditor.key t.editor);
  Inputs.Text.set_contents t.input_structure (VersionEditor.structure t.editor);
  Inputs.Text.set_contents t.input_remark (VersionEditor.remark t.editor);
  Inputs.Text.set_contents t.input_disambiguation (VersionEditor.disambiguation t.editor);
  Inputs.Textarea.set_contents t.input_content (VersionEditor.content t.editor)

let make_tune_modal editor content page =
  let modal_bg = Html.createDiv (Page.document page) in
  let tune_modal = Html.createDiv (Page.document page) in
  let interface =
    TuneEditorInterface.create page
      ~on_save:(fun slug ->
          Page.remove_modal page modal_bg;
          Dom.removeChild content modal_bg;
          Lwt.on_success (VersionEditor.set_tune editor slug) (fun () -> Page.refresh page))
  in
  Dom.appendChild tune_modal (TuneEditorInterface.contents interface);
  tune_modal##.classList##add (js "modal-window");
  modal_bg##.classList##add (js "modal-background");
  Dom.appendChild modal_bg tune_modal;
  Dom.appendChild content modal_bg;
  Page.register_modal page
    ~element:modal_bg
    ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg)
    ~on_refresh:(fun () -> TuneEditorInterface.refresh interface)
    ~targets:[tune_modal]

let make_tune_search_result editor page score =
  let tune = Score.value score in
  let score = score.Score.score in
  let%lwt name = Tune.name tune in
  let%lwt slug = Tune.slug tune in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success
            (VersionEditor.set_tune editor slug)
            (fun () -> Page.refresh page))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
        Table.Cell.text ~text:(Lwt.return name) page]
      page
  in
  Lwt.return row

let make_arranger_modal editor content page =
  let modal_bg = Html.createDiv (Page.document page) in
  let arranger_modal = Html.createDiv (Page.document page) in
  let interface =
    PersonEditorInterface.create page
      ~on_save:(fun slug ->
          Page.remove_modal page modal_bg;
          Dom.removeChild content modal_bg;
          Lwt.on_success (VersionEditor.set_arranger editor slug) (fun () -> Page.refresh page))
  in
  Dom.appendChild arranger_modal (PersonEditorInterface.contents interface);
  arranger_modal##.classList##add (js "modal-window");
  modal_bg##.classList##add (js "modal-background");
  Dom.appendChild modal_bg arranger_modal;
  Dom.appendChild content modal_bg;
  Page.register_modal page
    ~element:modal_bg
    ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg)
    ~on_refresh:(fun () -> PersonEditorInterface.refresh interface)
    ~targets:[arranger_modal]

let make_arranger_search_result editor page score =
  let arranger = Score.value score in
  let score = score.Score.score in
  let%lwt name = Person.line arranger in
  let%lwt slug = Person.slug arranger in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success
            (VersionEditor.set_arranger editor slug)
            (fun () -> Page.refresh page))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
        Table.Cell.text ~text:(Lwt.return name) page]
      page
  in
  Lwt.return row

let create page =
  let document = Page.document page in
  document##.title := js "Add a Tune | Dancelor";

  let editor = VersionEditor.create () in
  let content = Dom_html.createDiv document in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Add a new tune") page in
  let form = Html.createForm document in
  let input_bars = Inputs.Text.create
      ~placeholder:"Number of bars"
      ~on_change:(fun bars -> VersionEditor.set_bars editor bars)
      page
  in
  let input_key = Inputs.Text.create
      ~placeholder:"Key"
      ~on_change:(fun key -> VersionEditor.set_key editor key)
      page
  in
  let input_structure = Inputs.Text.create
      ~placeholder:"Structure of the tune (AABB, ABAB, ...)"
      ~on_change:(fun s -> VersionEditor.set_structure editor s)
      page
  in
  let input_remark = Inputs.Text.create
      ~placeholder:"Additional information about this version (origin, ...)"
      ~on_change:(fun r -> VersionEditor.set_remark editor r)
      page
  in
  let input_disambiguation = Inputs.Text.create
      ~placeholder:"Disambiguation information if this is a new version (optional)"
      ~on_change:(fun r -> VersionEditor.set_disambiguation editor r)
      page
  in
  let input_content = Inputs.Textarea.create
      ~placeholder:"Lilypond of the tune"
      ~on_change:(fun content -> VersionEditor.set_content editor content)
      page
  in

  let tune_search =
    let main_section =
      SearchBar.Section.create
        ~default:(Table.Row.create
                    ~on_click:(fun () -> make_tune_modal editor content page)
                    ~cells:[
                      Table.Cell.text ~text:(Lwt.return "  +") page;
                      Table.Cell.text ~text:(Lwt.return "Create a new associated tune") page]
                    page)
        ~search:(fun input ->
            let%rlwt formula = Lwt.return @@ Result.map_error List.singleton @@ Tune.Filter.from_string input in
            let%lwt results =
              Tune.search ~threshold:0.4
                ~pagination:Pagination.{start = 0; end_ = 10} formula
            in
            Lwt.return_ok results)
        ~make_result:(fun score -> make_tune_search_result editor page score)
        page
    in
    SearchBar.create
      ~placeholder:"Associated Tune (Magic Search)"
      ~sections:[main_section]
      page
  in
  Inputs.Text.on_focus (SearchBar.bar tune_search) (fun b ->
      if b then begin
        Inputs.Text.erase (SearchBar.bar tune_search);
        VersionEditor.remove_tune editor;
        Page.refresh page
      end);

  let arranger_search =
    let main_section =
      SearchBar.Section.create
        ~default:(Table.Row.create
                    ~on_click:(fun () -> make_arranger_modal editor content page)
                    ~cells:[
                      Table.Cell.text ~text:(Lwt.return "  +") page;
                      Table.Cell.text ~text:(Lwt.return "Create a new arranger") page]
                    page)
        ~search:(fun input ->
            let%rlwt formula = Lwt.return @@ Result.map_error List.singleton @@ Person.Filter.from_string input in
            let%lwt results =
              Person.search ~threshold:0.4
                ~pagination:Pagination.{start = 0; end_ = 10} formula
            in
            Lwt.return_ok results)
        ~make_result:(fun score -> make_arranger_search_result editor page score)
        page
    in
    SearchBar.create
      ~placeholder:"Arranger, if different from the tune author (optional)"
      ~sections:[main_section]
      page
  in
  Inputs.Text.on_focus (SearchBar.bar arranger_search) (fun b ->
      if b then begin
        Inputs.Text.erase (SearchBar.bar arranger_search);
        VersionEditor.remove_arranger editor;
        Page.refresh page
      end);

  let t =
    {page; editor; content; tune_search; input_bars; input_key; input_structure; arranger_search; input_remark; input_disambiguation; input_content}
  in

  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
          let b1, b2, b3, b4, b5 =
            Inputs.Text.check (SearchBar.bar t.tune_search)
              (fun _ -> VersionEditor.tune t.editor <> None),
            Inputs.Text.check input_bars
              (fun str -> try int_of_string str > 0 with _ -> false),
            Inputs.Text.check input_key
              (fun str -> try Music.key_of_string str |> ignore; true with _ -> false),
            Inputs.Text.check input_structure (fun str -> str <> ""),
            Inputs.Textarea.check input_content (fun str -> str <> "")
          in
          if b1 && b2 && b3 && b4 && b5 then (
            Lwt.on_success (VersionEditor.submit editor) (fun version ->
                Lwt.on_success (Version.slug version) (fun slug ->
                    let href = PageRouter.(path (Version slug)) in
                    Html.window##.location##.href := js href))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
          if Html.window##confirm (js "Clear the version?") |> Js.to_bool then begin
            VersionEditor.clear editor;
            refresh t;
            Inputs.Text.set_valid (SearchBar.bar t.tune_search) true;
            Inputs.Text.set_valid input_bars true;
            Inputs.Text.set_valid input_key true;
            Inputs.Text.set_valid input_structure true;
            Inputs.Textarea.set_valid input_content true
          end)
      page
  in

  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);

  Dom.appendChild form (SearchBar.root tune_search);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (Inputs.Text.root input_bars);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (Inputs.Text.root input_key);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (Inputs.Text.root input_structure);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (SearchBar.root arranger_search);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (Inputs.Text.root input_remark);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (Inputs.Text.root input_disambiguation);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form (Inputs.Textarea.root input_content);
  Dom.appendChild form (Html.createBr document);
  Dom.appendChild form submit;

  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content form;

  t


let contents t =
  t.content

let init t =
  refresh t
