open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_elements
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    composer : SetEditor.t;
    content : Html.divElement Js.t;
    warnings_area : Html.divElement Js.t;
    input_name : Inputs.Text.t;
    input_kind : Inputs.Text.t;
    conceptor_search : SearchBar.t;
    book_search : SearchBar.t;
    versions_area : Html.divElement Js.t;
    version_search : SearchBar.t;
    input_order : Inputs.Text.t;
  }

let get_duplicated_tunes t book =
  let book_tunes = Hashtbl.create 0 in

  let%lwt sets = Book.unique_sets_from_contents book in
  let%lwt standalone_versions = Book.versions_from_contents book in
  let register_tune tune = Hashtbl.add book_tunes tune true in
  (* Register standalone tunes *)
  Lwt_list.iter_s
    (fun v ->
       let%lwt tune = Version.tune v in
       register_tune tune;
       Lwt.return ())
    standalone_versions;%lwt
  (* Register tunes in sets *)
  Lwt_list.iter_s
    (fun set ->
       let%lwt contents = Set.contents set in
       let versions = List.map fst contents in
       Lwt_list.iter_s
         (fun v ->
            let%lwt tune = Version.tune v in
            register_tune tune;
            Lwt.return ())
         versions)
    sets;%lwt

  let set_tunes = SetEditor.list_tunes t.composer in
  Lwt.return (List.fold_left
                (fun acc tune -> if Hashtbl.mem book_tunes tune then tune :: acc else acc)
                []
                set_tunes)


let display_warnings t =
  let open Dancelor_client_html in
  (* Only open a warnings div if there are warnings *)
  match SetEditor.for_book t.composer with
  | None -> Lwt.return []
  | Some bk ->
    let%lwt duplicated_tunes = get_duplicated_tunes t bk in
    match duplicated_tunes with
    | [] -> Lwt.return []
    | duplicated_tunes ->
      let display_duplicated_warning tune =
        li [
          txt "Tune “";
          span (Formatters.Tune.name tune);
          txt "” already appears in book ";
          span (Formatters.Book.short_title bk);
        ]
      in
      Lwt.return [
        div ~a:[a_class ["warning"]] [
          ul (List.map display_duplicated_warning duplicated_tunes)
        ];
        br ();
      ]

let make_version_subwindow t index version =
  let subwin = Html.createDiv (Page.document t.page) in
  subwin##.classList##add (js "subwindow");
  let toolbar = Html.createDiv (Page.document t.page) in
  toolbar##.classList##add (js "toolbar");
  let title = Text.Heading.h3_static ~text:(Lwt.return @@ Tune.name version.SetEditor.tune) t.page in
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
  let source = Lwt.return (ApiRouter.path_versionSvg version.SetEditor.slug) in
  let img = Image.create ~source t.page in
  Dom.appendChild subwin (Image.root img);
  subwin

let refresh t =
  Inputs.Text.set_contents t.input_name (SetEditor.name t.composer);
  Inputs.Text.set_contents t.input_kind (SetEditor.kind t.composer);
  Inputs.Text.set_contents t.input_order (SetEditor.order t.composer);
  Inputs.Text.set_contents (SearchBar.bar t.conceptor_search) (Option.fold ~none:"" ~some:Person.name (SetEditor.conceptor t.composer));
  Inputs.Text.set_contents (SearchBar.bar t.book_search) (Option.fold ~none:"" ~some:Book.title (SetEditor.for_book t.composer));
  JsHelpers.clear_children t.versions_area;
  SetEditor.iter t.composer (fun i version ->
      let subwin = make_version_subwindow t i version in
      Dom.appendChild t.versions_area (Html.createBr (Page.document t.page));
      Dom.appendChild t.versions_area subwin);
  JsHelpers.clear_children t.warnings_area;
  Dom.appendChild t.warnings_area
    Dancelor_client_html.(To_dom.of_div (L.div (display_warnings t)))

let make_version_search_result composer page version =
  let slug = Version.slug version in
  let bars = Version.bars version in
  let structure = Version.structure version in
  let%lwt tune = Version.tune version in
  let kind = Tune.kind tune in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success (SetEditor.add composer slug) (fun () -> Page.refresh page; SetEditor.save composer))
      ~cells:[
        Table.Cell.create ~content:(
          Dancelor_client_html.to_old_style
            (Dancelor_client_formatters.Version.name_disambiguation_and_sources ~link:false version)
        ) page;
        Table.Cell.create ~content:(
          Dancelor_client_html.to_old_style
            (Dancelor_client_formatters.Version.composer_and_arranger ~short:true ~link:false version)
        ) page;
        Table.Cell.text ~text:(Lwt.return (string_of_int bars)) page;
        Table.Cell.text ~text:(Lwt.return (Kind.Base.to_pretty_string ~capitalised:true kind)) page;
        Table.Cell.text ~text:(Lwt.return structure) page]
      page
  in
  Lwt.return row

let make_person_modal composer content page =
  let modal_bg = Html.createDiv (Page.document page) in
  let persons_modal = Html.createDiv (Page.document page) in
  let interface =
    PersonEditorInterface.create page
      ~on_save:(fun slug ->
          Page.remove_modal page modal_bg;
          Dom.removeChild content modal_bg;
          Lwt.on_success (SetEditor.set_conceptor composer slug) (fun () -> Page.refresh page))
  in
  Dom.appendChild persons_modal (PersonEditorInterface.contents interface);
  persons_modal##.classList##add (js "modal-window");
  modal_bg##.classList##add (js "modal-background");
  Dom.appendChild modal_bg persons_modal;
  Dom.appendChild content modal_bg;
  Page.register_modal page
    ~element:modal_bg
    ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg)
    ~on_refresh:(fun () -> PersonEditorInterface.refresh interface)
    ~targets:[persons_modal]

let make_conceptor_search_result composer page conceptor =
  let name = Person.name conceptor in
  let slug = Person.slug conceptor in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success
            (SetEditor.set_conceptor composer slug)
            (fun () -> Page.refresh page; SetEditor.save composer))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return name) page]
      page
  in
  Lwt.return row

let make_book_search_result composer page book =
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success
            (SetEditor.set_for_book composer (Book.slug book))
            (fun () -> Page.refresh page; SetEditor.save composer))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return @@ Book.title book) page]
      page
  in
  Lwt.return row

let create page =
  (Page.document page)##.title := js "Add a set | Dancelor";

  let composer = SetEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let warnings_area = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Add a set") page in
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
  let conceptor_search =
    let main_section =
      SearchBar.Section.create
        ~default:(Table.Row.create
                    ~on_click:(fun () -> make_person_modal composer content page)
                    ~cells:[
                      Table.Cell.text ~icon:"add_circle" ~colspan:0 ~text:(Lwt.return "Create a new person") page]
                    page)
        ~search:(fun input ->
            let%rlwt formula = Lwt.return @@ Person.Filter.from_string input in
            let%lwt results =
              Person.search' ~threshold:0.4
                ~slice: (Slice.make ~start:0 ~end_excl:10 ()) formula
            in
            Lwt.return_ok results)
        ~make_result:(make_conceptor_search_result composer page)
        page
    in
    SearchBar.create
      ~placeholder:"Deviser (Magic Search)"
      ~sections:[main_section]
      page
  in
  let book_search =
    let main_section =
      SearchBar.Section.create
        ~search:(fun input ->
            let%rlwt formula = Lwt.return @@ Book.Filter.from_string input in
            let%lwt results =
              Book.search' ~threshold:0.4
                ~slice: (Slice.make ~start:0 ~end_excl:10 ()) formula
            in
            Lwt.return_ok results)
        ~make_result:(make_book_search_result composer page)
        page
    in
    SearchBar.create
      ~placeholder:"Book (Magic Search)"
      ~sections:[main_section]
      page
  in
  let versions_area = Html.createDiv (Page.document page) in
  let version_search =
    let main_section =
      SearchBar.Section.create
        ~search:(fun input ->
            let%rlwt formula = Lwt.return @@ Version.Filter.from_string input in
            let%lwt results =
              Version.search' ~threshold:0.4
                ~slice: (Slice.make ~start:0 ~end_excl:10 ()) formula
            in
            Lwt.return_ok results)
        ~make_result:(make_version_search_result composer page)
        page
    in
    SearchBar.create
      ~placeholder:"Add tune (Magic Search)"
      ~sections:[main_section]
      page
  in
  Inputs.Text.on_focus (SearchBar.bar conceptor_search) (fun b ->
      if b then begin
        Inputs.Text.erase (SearchBar.bar conceptor_search);
        SetEditor.remove_conceptor composer;
        Page.refresh page
      end);
  Inputs.Text.on_focus (SearchBar.bar book_search) (fun b ->
      if b then begin
        Inputs.Text.erase (SearchBar.bar book_search);
        SetEditor.remove_for_book composer;
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
    {page; composer; content; warnings_area; versions_area; conceptor_search;
     book_search; version_search; input_name; input_kind; input_order}
  in
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
          let b1, b2, b3, b4, b5 =
            Inputs.Text.check t.input_kind (Option.is_some % Kind.Dance.of_string_opt),
            Inputs.Text.check t.input_name ((<>) ""),
            Inputs.Text.check (SearchBar.bar t.version_search)
              (fun _ -> SetEditor.count t.composer > 0),
            Inputs.Text.check (SearchBar.bar t.conceptor_search)
              (fun _ -> SetEditor.conceptor t.composer <> None),
            Inputs.Text.check t.input_order
              (SetOrder.check ~number:(SetEditor.count t.composer))
          in
          if b1 && b2 && b3 && b4 && b5 then (
            Lwt.on_success (SetEditor.submit composer) (fun set ->
                Html.window##.location##.href := js (PageRouter.path_set @@ Set.slug set))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"cancel" ~text:"Clear"
      ~on_click:(fun () ->
          if Html.window##confirm (js "Clear the composer?") |> Js.to_bool then begin
            SetEditor.clear composer;
            SetEditor.erase_storage composer;
            refresh t;
            Inputs.Text.set_valid input_kind true;
            Inputs.Text.set_valid input_name true;
            Inputs.Text.set_valid (SearchBar.bar version_search) true;
            Inputs.Text.set_valid (SearchBar.bar conceptor_search) true;
            Inputs.Text.set_valid input_order true
          end)
      page
  in

  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_kind);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root conceptor_search);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root book_search);
  Dom.appendChild form versions_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root version_search);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_order);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content warnings_area;
  Dom.appendChild content form;
  Lwt.on_success (SetEditor.load composer) (fun () -> refresh t);
  t

let contents t =
  t.content

let init t =
  refresh t
