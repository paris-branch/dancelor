open Nes
open Js_of_ocaml
open Dancelor_client_model
open Dancelor_client_utils
open Dancelor_client_elements
open Dancelor_common

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    composer : SetEditor.t;
    content : Html.divElement Js.t;
    input_name : Inputs.Text.t;
    input_kind : Inputs.Text.t;
    deviser_search : SearchBar.t;
    versions_area : Html.divElement Js.t;
    version_search : SearchBar.t;
    input_order : Inputs.Text.t;
  }

let make_version_subwindow t index version =
  let subwin = Html.createDiv (Page.document t.page) in
  subwin##.classList##add (js "subwindow");
  let toolbar = Html.createDiv (Page.document t.page) in
  toolbar##.classList##add (js "toolbar");
  let title = Text.Heading.h3_static ~text:(Tune.name version.SetEditor.tune) t.page in
  Dom.appendChild toolbar (Text.Heading.root title);
  let buttons = Html.createUl (Page.document t.page) in
  let down, up, del =
    Inputs.Button.create
      ~on_click:(fun () ->
          SetEditor.move_down t.composer index;
          SetEditor.save t.composer;
          Page.refresh t.page)
      ~icon:"chevron-down"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
          SetEditor.move_up t.composer index;
          SetEditor.save t.composer;
          Page.refresh t.page)
      ~icon:"chevron-up"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
          SetEditor.remove t.composer index;
          SetEditor.save t.composer;
          Page.refresh t.page)
      ~kind:Inputs.Button.Kind.Danger
      ~icon:"times"
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
  let source =
    spf "/%s%s"
      Constant.api_prefix
      (Router.path_of_controller (Router.VersionSvg version.SetEditor.slug) |> snd)
    |> Lwt.return
  in
  let img = Image.create ~source t.page in
  Dom.appendChild subwin (Image.root img);
  subwin

let refresh t =
  Inputs.Text.set_contents t.input_name (SetEditor.name t.composer);
  Inputs.Text.set_contents t.input_kind (SetEditor.kind t.composer);
  Inputs.Text.set_contents t.input_order (SetEditor.order t.composer);
  begin match SetEditor.deviser t.composer with
    | None -> Inputs.Text.set_contents (SearchBar.bar t.deviser_search) ""
    | Some cr ->
      let name = Credit.line cr in
      Lwt.on_success name (fun name ->
          Inputs.Text.set_contents (SearchBar.bar t.deviser_search) name)
  end;
  Helpers.clear_children t.versions_area;
  SetEditor.iter t.composer (fun i version ->
      let subwin = make_version_subwindow t i version in
      Dom.appendChild t.versions_area (Html.createBr (Page.document t.page));
      Dom.appendChild t.versions_area subwin)

let make_version_search_result composer page score =
  let version = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Version.slug version in
  let%lwt bars = Version.bars version in
  let%lwt structure = Version.structure version in
  let%lwt tune = Version.tune version in
  let%lwt kind = Tune.kind tune in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success (SetEditor.add composer slug) (fun () -> Page.refresh page; SetEditor.save composer))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
        Table.Cell.create ~content:(
          Dancelor_client_formatters.Version.name_disambiguation_and_sources ~link:false version
          >|=| Dancelor_client_html.nodes_to_dom_nodes (Page.document page)
        ) page;
        Table.Cell.create ~content:(
          Dancelor_client_formatters.Version.author_and_arranger ~link:false version
          >|=| Dancelor_client_html.nodes_to_dom_nodes (Page.document page)
        ) page;
        Table.Cell.text ~text:(Lwt.return (string_of_int bars)) page;
        Table.Cell.text ~text:(Lwt.return (Kind.base_to_pretty_string ~capitalised:true kind)) page;
        Table.Cell.text ~text:(Lwt.return structure) page]
      page
  in
  Lwt.return row

let make_credit_modal composer content page =
  let modal_bg = Html.createDiv (Page.document page) in
  let credits_modal = Html.createDiv (Page.document page) in
  let interface =
    CreditEditorInterface.create page
      ~on_save:(fun slug ->
          Page.remove_modal page modal_bg;
          Dom.removeChild content modal_bg;
          Lwt.on_success (SetEditor.set_deviser composer slug) (fun () -> Page.refresh page))
  in
  Dom.appendChild credits_modal (CreditEditorInterface.contents interface);
  credits_modal##.classList##add (js "modal-window");
  modal_bg##.classList##add (js "modal-background");
  Dom.appendChild modal_bg credits_modal;
  Dom.appendChild content modal_bg;
  Page.register_modal page
    ~element:modal_bg
    ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg)
    ~on_refresh:(fun () -> CreditEditorInterface.refresh interface)
    ~targets:[credits_modal]

let make_deviser_search_result composer page score =
  let deviser = Score.value score in
  let score = score.Score.score in
  let%lwt name = Credit.line deviser in
  let%lwt slug = Credit.slug deviser in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success
            (SetEditor.set_deviser composer slug)
            (fun () -> Page.refresh page; SetEditor.save composer))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
        Table.Cell.text ~text:(Lwt.return name) page]
      page
  in
  Lwt.return row

let create page =
  (Page.document page)##.title := js "Compose a Set | Dancelor";

  let composer = SetEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Compose a Set") page in
  let form = Html.createForm (Page.document page) in
  let input_name = Inputs.Text.create
      ~placeholder:"Name"
      ~on_change:(fun name ->
          SetEditor.set_name composer name;
          SetEditor.save composer)
      page
  in
  let input_kind = Inputs.Text.create
      ~placeholder:"Kind (eg. 8x32R)"
      ~on_change:(fun kind ->
          SetEditor.set_kind composer kind;
          SetEditor.save composer)
      page
  in
  let deviser_search =
    let main_section =
      SearchBar.Section.create
        ~default:(Table.Row.create
                    ~on_click:(fun () -> make_credit_modal composer content page)
                    ~cells:[
                      Table.Cell.text ~text:(Lwt.return "  +") page;
                      Table.Cell.text ~text:(Lwt.return "Create a new deviser") page]
                    page)
        ~search:(fun input ->
            let%rlwt formula = Lwt.return (CreditFilter.raw input) in
            let%lwt results =
              Credit.search ~threshold:0.4
                ~pagination:Pagination.{start = 0; end_ = 10} formula
            in
            Lwt.return_ok results)
        ~make_result:(fun score -> make_deviser_search_result composer page score)
        page
    in
    SearchBar.create
      ~placeholder:"Deviser (Magic Search)"
      ~sections:[main_section]
      page
  in
  let versions_area = Html.createDiv (Page.document page) in
  let version_search =
    let main_section =
      SearchBar.Section.create
        ~search:(fun input ->
            let%rlwt formula = Lwt.return (VersionFilter.raw input) in
            let%lwt results =
              Version.search ~threshold:0.4
                ~pagination:Pagination.{start = 0; end_ = 10} formula
            in
            Lwt.return_ok results)
        ~make_result:(fun score -> make_version_search_result composer page score)
        page
    in
    SearchBar.create
      ~placeholder:"Add tune (Magic Search)"
      ~sections:[main_section]
      page
  in
  Inputs.Text.on_focus (SearchBar.bar deviser_search) (fun b ->
      if b then begin
        Inputs.Text.erase (SearchBar.bar deviser_search);
        SetEditor.remove_deviser composer;
        Page.refresh page
      end);
  let input_order = Inputs.Text.create
      ~placeholder:"Order (eg. 1,2,3,4,2,3,4,1)"
      ~on_change:(fun order ->
          SetEditor.set_order composer order;
          SetEditor.save composer)
      page
  in
  let t =
    {page; composer; content; versions_area; deviser_search;
     version_search; input_name; input_kind; input_order}
  in
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
          let b1, b2, b3, b4, b5 =
            Inputs.Text.check t.input_kind Kind.check_dance,
            Inputs.Text.check t.input_name (fun str -> str <> ""),
            Inputs.Text.check (SearchBar.bar t.version_search)
              (fun _ -> SetEditor.count t.composer > 0),
            Inputs.Text.check (SearchBar.bar t.deviser_search)
              (fun _ -> SetEditor.deviser t.composer <> None),
            Inputs.Text.check t.input_order
              (SetOrder.check ~number:(SetEditor.count t.composer))
          in
          if b1 && b2 && b3 && b4 && b5 then (
            Lwt.on_success (SetEditor.submit composer) (fun set ->
                Lwt.on_success (Set.slug set) (fun slug ->
                    let href = Router.path_of_controller (Router.Set slug) |> snd in
                    Html.window##.location##.href := js href))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
          if Html.window##confirm (js "Clear the composer?") |> Js.to_bool then begin
            SetEditor.clear composer;
            SetEditor.erase_storage composer;
            refresh t;
            Inputs.Text.set_valid input_kind true;
            Inputs.Text.set_valid input_name true;
            Inputs.Text.set_valid (SearchBar.bar version_search) true;
            Inputs.Text.set_valid (SearchBar.bar deviser_search) true;
            Inputs.Text.set_valid input_order true
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
  Dom.appendChild form (Inputs.Text.root input_kind);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root deviser_search);
  Dom.appendChild form versions_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root version_search);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_order);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content form;
  Lwt.on_success (SetEditor.load composer) (fun () -> refresh t);
  t

let contents t =
  t.content

let init t =
  refresh t
