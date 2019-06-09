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
  persons_area : Html.divElement Js.t;
  input_name : Inputs.Text.t;
  search_bar : Person.t SearchBar.t;
  mutable persons_inputs : Inputs.Text.t list;
}

let make_person_subdiv t index person =
  let root = Html.createDiv (Page.document t.page) in
  Style.set ~display:"flex" root;
  let buttons = Html.createUl (Page.document t.page) in
  Style.set ~display:"flex" buttons;
  let down, up, del =
    Inputs.Button.create
      ~on_click:(fun () ->
        CreditEditor.move_down t.editor index;
        Page.refresh t.page)
      ~icon:"chevron-down"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        CreditEditor.move_up t.editor index;
        Page.refresh t.page)
      ~icon:"chevron-up"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        CreditEditor.remove t.editor index;
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
  Dom.appendChild root buttons;
  begin match person with
  | `Edit name ->
    let input_name =
      Inputs.Text.create
        ~default:"Enter name"
        ~on_change:(CreditEditor.set_field t.editor index)
        t.page
    in
    t.persons_inputs <- input_name :: t.persons_inputs;
    Inputs.Text.set_contents input_name name;
    Style.set ~margin:"0 0 0 5pt" (Inputs.Text.root input_name);
    Dom.appendChild root (Inputs.Text.root input_name)
  | `Person person ->
    let href =
      Helpers.build_path ~route:(Router.Person person.CreditEditor.slug) ()
      |> Lwt.return
    in
    let link =
      Text.Link.create
        ~href
        ~text:(Person.name person.CreditEditor.person) t.page
    in
    Style.set ~margin:"0 0 0 5pt" (Text.Link.root link);
    Dom.appendChild root (Text.Link.root link)
  end;
  root

let refresh t =
  Inputs.Text.set_contents t.input_name (CreditEditor.name t.editor);
  Helpers.clear_children t.persons_area;
  t.persons_inputs <- [];
  CreditEditor.iter t.editor (fun i person ->
    let subwin = make_person_subdiv t i person in
    Dom.appendChild t.persons_area (Html.createBr (Page.document t.page));
    Dom.appendChild t.persons_area subwin)
  |> ignore

let create ?on_save page =
  let editor = CreditEditor.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h1 ~text:(Lwt.return "Create a Credit") page in
  let form = Html.createForm (Page.document page) in
  let input_name = Inputs.Text.create
    ~default:"Display name"
    ~on_change:(fun name -> CreditEditor.set_name editor name)
    page
  in
  let persons_area = Html.createDiv (Page.document page) in
  let search_bar =
    let main_section = 
      SearchBar.Section.create
        ~search:(fun input -> Person.search ~threshold:0.4 ~pagination:Pagination.{start = 0; end_ = 10} input)
        ~default:(Table.Row.create
          ~cells:[
            Table.Cell.text ~text:(Lwt.return "  +") page;
            Table.Cell.text ~text:(Lwt.return "Create a new person") page]
          ~on_click:(fun () ->
            Lwt.on_success
              (CreditEditor.add editor (`New ""))
              (fun () -> Page.refresh page))
          page)
        ~make_result:(fun score ->
          let person = Score.value score in
          let score = score.Score.score in
          let%lwt slug = Person.slug person in
          let%lwt name = Person.name person in
          let row = Table.Row.create
            ~on_click:(fun () ->
              Lwt.on_success
                (CreditEditor.add editor (`Slug slug))
                (fun () -> Page.refresh page))
            ~cells:[
              Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) page;
              Table.Cell.text ~text:(Lwt.return name) page]
            page
          in
          Lwt.return row)
        page
    in
    SearchBar.create
      ~placeholder:"Search for a person"
      ~sections:[main_section]
      page
  in
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let t = {page; editor; content; persons_area; input_name; search_bar; persons_inputs = []} in
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1, b2, b3 =
          Inputs.Text.check input_name (fun str -> str <> ""),
          Inputs.Text.check (SearchBar.bar search_bar) (fun _ -> CreditEditor.count editor > 0),
          List.for_all (fun input -> Inputs.Text.check input (fun str -> str <> ""))
            t.persons_inputs
        in
        if b1 && b2 && b3 then (
          Lwt.on_success (CreditEditor.submit editor) (fun credit ->
          Lwt.on_success (Credit.slug credit) (fun slug ->
          begin match on_save with
          | None ->
            let href = Router.path_of_controller (Router.Credit slug) |> snd in
            Html.window##.location##.href := js href
          | Some cb -> cb slug
          end))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
        if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin
          CreditEditor.clear editor;
          Page.refresh page
        end)
      page
  in
  Dom.appendChild submit (Inputs.Button.root save);
  Dom.appendChild submit (Inputs.Button.root clear);
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr (Page.document page));
  Dom.appendChild content (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form persons_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (SearchBar.root search_bar);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content form;
  t

let contents t =
  t.content
