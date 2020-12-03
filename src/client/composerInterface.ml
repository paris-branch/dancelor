open Nes
open Js_of_ocaml
open Dancelor_client_model
open Dancelor_client_elements
open Dancelor_common

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  composer : Composer.t;
  content : Html.divElement Js.t;
  input_name : Inputs.Text.t;
  input_kind : Inputs.Text.t;
  deviser_search : SearchBar.t;
  versions_area : Html.divElement Js.t;
  version_search : SearchBar.t;
}

let make_version_subwindow t index version =
  let subwin = Html.createDiv (Page.document t.page) in
  subwin##.classList##add (js "subwindow");
  let toolbar = Html.createDiv (Page.document t.page) in
  toolbar##.classList##add (js "toolbar");
  let title = Text.Heading.h3_static ~text:(Tune.name version.Composer.tune) t.page in
  Dom.appendChild toolbar (Text.Heading.root title);
  let buttons = Html.createUl (Page.document t.page) in
  let down, up, del =
    Inputs.Button.create
      ~on_click:(fun () ->
        Composer.move_down t.composer index;
        Composer.save t.composer;
        Page.refresh t.page)
      ~icon:"chevron-down"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        Composer.move_up t.composer index;
        Composer.save t.composer;
        Page.refresh t.page)
      ~icon:"chevron-up"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        Composer.remove t.composer index;
        Composer.save t.composer;
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
      (Router.path_of_controller (Router.VersionSvg version.Composer.slug) |> snd)
    |> Lwt.return
  in
  let img = Image.create ~source t.page in
  Dom.appendChild subwin (Image.root img);
  subwin

let refresh t =
  Inputs.Text.set_contents t.input_name (Composer.name t.composer);
  Inputs.Text.set_contents t.input_kind (Composer.kind t.composer);
  begin match Composer.deviser t.composer with
  | None -> Inputs.Text.set_contents (SearchBar.bar t.deviser_search) ""
  | Some cr ->
    let name = Credit.line cr in
    Lwt.on_success name (fun name ->
      Inputs.Text.set_contents (SearchBar.bar t.deviser_search) name)
  end;
  Helpers.clear_children t.versions_area;
  Composer.iter t.composer (fun i version ->
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
  let%lwt name = Tune.name tune in
  let%lwt disambiguation = Version.disambiguation version in
  let%lwt kind = Tune.kind tune in
  let row = Table.Row.create
    ~on_click:(fun () ->
      Lwt.on_success (Composer.add composer slug) (fun () -> Page.refresh page; Composer.save composer))
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
      Table.Cell.text ~text:(Lwt.return name) page;
      Table.Cell.text ~text:(Lwt.return disambiguation) page;
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
        Lwt.on_success (Composer.set_deviser composer slug) (fun () -> Page.refresh page))
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
        (Composer.set_deviser composer slug)
        (fun () -> Page.refresh page; Composer.save composer))
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
      Table.Cell.text ~text:(Lwt.return name) page]
    page
  in
  Lwt.return row

let create page =
  let composer = Composer.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Compose a Set") page in
  let form = Html.createForm (Page.document page) in
  let input_name = Inputs.Text.create
    ~default:"Set Name"
    ~on_change:(fun name ->
      Composer.set_name composer name;
      Composer.save composer)
    page
  in
  let input_kind = Inputs.Text.create
    ~default:"Set Kind (e.g. 8x32R)"
    ~on_change:(fun kind ->
      Composer.set_kind composer kind;
      Composer.save composer)
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
        ~search:(fun input -> Credit.search ~threshold:0.4 ~pagination:Pagination.{start = 0; end_ = 10} input)
        ~make_result:(fun score -> make_deviser_search_result composer page score)
        page
    in
    SearchBar.create
      ~placeholder:"Select a Deviser"
      ~sections:[main_section]
      page
  in
  let versions_area = Html.createDiv (Page.document page) in
  let version_search =
    let main_section =
      SearchBar.Section.create
        ~search:(fun input -> Version.search ~threshold:0.4 ~pagination:Pagination.{start = 0; end_ = 10} input)
        ~make_result:(fun score -> make_version_search_result composer page score)
        page
    in
    SearchBar.create
      ~placeholder:"Search for a version"
      ~sections:[main_section]
      page
  in
  Inputs.Text.on_focus (SearchBar.bar deviser_search) (fun b ->
    if b then begin
      Inputs.Text.erase (SearchBar.bar deviser_search);
      Composer.remove_deviser composer;
      Page.refresh page
    end);
  let t =
    {page; composer; content; versions_area; deviser_search;
    version_search; input_name; input_kind}
  in
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1, b2, b3, b4 =
          Inputs.Text.check t.input_kind Kind.check_dance,
          Inputs.Text.check t.input_name (fun str -> str <> ""),
          Inputs.Text.check (SearchBar.bar t.version_search)
            (fun _ -> Composer.count t.composer > 0),
          Inputs.Text.check (SearchBar.bar t.deviser_search)
            (fun _ -> Composer.deviser t.composer <> None)
        in
        if b1 && b2 && b3 && b4 then (
          Lwt.on_success (Composer.submit composer) (fun set ->
          Lwt.on_success (Set.slug set) (fun slug ->
          let href = Router.path_of_controller (Router.Set slug) |> snd in
          Html.window##.location##.href := js href))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
        if Html.window##confirm (js "Clear the composer?") |> Js.to_bool then begin
          Composer.clear composer;
          Composer.erase_storage composer;
          refresh t
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
  Dom.appendChild form submit;
  Dom.appendChild content form;
  Lwt.on_success (Composer.load composer) (fun () -> refresh t);
  t

let contents t =
  t.content

let init t =
  refresh t
