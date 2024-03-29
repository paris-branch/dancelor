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
    editor : DanceEditor.t;
    content : Html.divElement Js.t;
    input_name : Inputs.Text.t;
    input_kind : Inputs.Text.t;
    input_date : Inputs.Text.t;
    deviser_search : SearchBar.t;
    input_two_chords : Inputs.Switch.t;
    input_scddb_id : Inputs.Text.t;
  }

let refresh t =
  Inputs.Text.set_contents t.input_name (DanceEditor.name t.editor);
  Inputs.Text.set_contents t.input_kind (DanceEditor.kind t.editor);
  Inputs.Text.set_contents t.input_date (DanceEditor.date t.editor);
  begin match DanceEditor.deviser t.editor with
    | None -> Inputs.Text.set_contents (SearchBar.bar t.deviser_search) ""
    | Some cr ->
      let name = Person.name cr in
      Inputs.Text.set_contents (SearchBar.bar t.deviser_search) name
  end;
  if DanceEditor.two_chords t.editor then
    Inputs.Switch.enable t.input_two_chords
  else Inputs.Switch.disable t.input_two_chords;
  Inputs.Text.set_contents t.input_scddb_id (DanceEditor.scddb_id t.editor)

let make_deviser_modal editor content page =
  let modal_bg = Html.createDiv (Page.document page) in
  let person_modal = Html.createDiv (Page.document page) in
  let interface =
    PersonEditorInterface.create page
      ~on_save:(fun slug ->
          Page.remove_modal page modal_bg;
          Dom.removeChild content modal_bg;
          Lwt.on_success (DanceEditor.set_deviser editor slug) (fun () -> Page.refresh page))
  in
  Dom.appendChild person_modal (PersonEditorInterface.contents interface);
  person_modal##.classList##add (js "modal-window");
  modal_bg##.classList##add (js "modal-background");
  Dom.appendChild modal_bg person_modal;
  Dom.appendChild content modal_bg;
  Page.register_modal page
    ~element:modal_bg
    ~on_unfocus:(fun () -> Dom.removeChild content modal_bg; Page.remove_modal page modal_bg)
    ~on_refresh:(fun () -> PersonEditorInterface.refresh interface)
    ~targets:[person_modal]

let make_deviser_search_result editor page deviser =
  let name = Person.name deviser in
  let slug = Person.slug deviser in
  let row = Table.Row.create
      ~on_click:(fun () ->
          Lwt.on_success
            (DanceEditor.set_deviser editor slug)
            (fun () -> Page.refresh page))
      ~cells:[
        Table.Cell.text ~text:(Lwt.return name) page]
      page
  in
  Lwt.return row

let create ?on_save page =
  let editor = DanceEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h2_static ~text:(Lwt.return "Add a Dance") page in
  let form = Html.createForm (Page.document page) in
  let input_name = Inputs.Text.create
      ~placeholder:"Name of the dance"
      ~on_change:(fun name -> DanceEditor.set_name editor name)
      page
  in
  let input_kind = Inputs.Text.create
      ~placeholder:"Kind of the dance (8x32R, 2x(16R + 16S), ...)"
      ~on_change:(fun kind -> DanceEditor.set_kind editor kind)
      page
  in
  let input_date = Inputs.Text.create
      ~placeholder:"Date of devising (eg. 2019 or 2012-03-14)"
      ~on_change:(fun date -> DanceEditor.set_date editor date)
      page
  in
  let input_scddb_id = Inputs.Text.create
      ~placeholder:"Strathspey Database link or id (optional)"
      ~on_change:(fun id -> DanceEditor.set_scddb_id editor id)
      page
  in

  let input_two_chords = Inputs.Switch.create
      ~text_after:" Two Chords?"
      ~id:"Two Chords"
      ~on_change:(fun b -> DanceEditor.set_two_chords editor b)
      page
  in

  let deviser_search =
    let main_section =
      SearchBar.Section.create
        ~default:(Table.Row.create
                    ~on_click:(fun () -> make_deviser_modal editor content page)
                    ~cells:[
                      Table.Cell.text ~colspan:0 ~icon:"add_circle" ~text:(Lwt.return "Create a new deviser") page]
                    page)
        ~search:(fun input ->
            let%rlwt formula = Lwt.return @@ Person.Filter.from_string input in
            let%lwt results = Person.search' ~threshold:0.4 ~slice:(Slice.make ~start:0 ~end_excl:10 ()) formula in
            Lwt.return_ok results)
        ~make_result:(make_deviser_search_result editor page)
        page
    in
    SearchBar.create
      ~placeholder:"Devisor (Magic Search)"
      ~sections:[main_section]
      page
  in

  Inputs.Text.on_focus (SearchBar.bar deviser_search) (fun b ->
      if b then begin
        Inputs.Text.erase (SearchBar.bar deviser_search);
        DanceEditor.remove_deviser editor;
        Page.refresh page
      end);

  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");

  let t = {page; editor; content; input_name; input_kind; input_date; deviser_search; input_two_chords; input_scddb_id} in

  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
          let b1, b2, b3, b4 =
            Inputs.Text.check input_name (fun str -> str <> ""),
            Inputs.Text.check input_kind (fun str -> try Kind.Dance.of_string str |> ignore; true with _ -> false),
            Inputs.Text.check input_date (fun str -> str = "" || try PartialDate.from_string str |> ignore; true with _ -> false),
            Inputs.Text.check input_scddb_id (fun str ->
                if str = "" then
                  true
                else
                  match int_of_string_opt str with
                  | Some _ -> true
                  | None ->
                    match SCDDB.dance_from_uri str with
                    | Ok _ -> true
                    | Error _ -> false
              )
          in
          if b1 && b2 && b3 && b4 then (
            Lwt.on_success (DanceEditor.submit editor) (fun dance ->
                let slug = Dance.slug dance in
                match on_save with
                | None -> Html.window##.location##.href := js (PageRouter.path_dance slug)
                | Some cb -> cb slug
              )))
      page
  in

  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"cancel" ~text:"Clear"
      ~on_click:(fun () ->
          if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin
            DanceEditor.clear editor;
            Page.refresh page;
            Inputs.Text.set_valid input_name true;
            Inputs.Text.set_valid input_kind true;
            Inputs.Text.set_valid input_date true;
            Inputs.Text.set_valid input_scddb_id true
          end)
      page
  in

  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);

  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_kind);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root deviser_search);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_date);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Switch.root input_two_chords);
  Dom.appendChild form (Html.createBr (Page.document page));
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
