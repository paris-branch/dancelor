open Js_of_ocaml
open Js_of_ocaml_lwt
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
  search_bar : Inputs.Text.t;
  search_results : Table.t;
  input_name : Inputs.Text.t;
  mutable persons_inputs : Inputs.Text.t list;
}


let rec make_person_subdiv t index person = 
  let root = Html.createDiv (Page.document t.page) in
  Style.set ~display:"flex" root;
  let buttons = Html.createUl (Page.document t.page) in
  Style.set ~display:"flex" buttons;
  let down, up, del = 
    Inputs.Button.create
      ~on_click:(fun () ->
        CreditEditor.move_down t.editor index;
        refresh t)
      ~icon:"chevron-down"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        CreditEditor.move_up t.editor index;
        refresh t)
      ~icon:"chevron-up"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        CreditEditor.remove t.editor index;
        refresh t)
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
      Dancelor_client_api.build_path ~route:(Router.Person person.CreditEditor.slug) ()
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

and refresh t =
  Inputs.Text.set_contents t.input_name (CreditEditor.name t.editor);
  while Js.to_bool t.persons_area##hasChildNodes do
    let child = Js.Opt.get t.persons_area##.firstChild (fun () -> assert false) in
    t.persons_area##removeChild child |> ignore
  done;
  t.persons_inputs <- [];
  CreditEditor.iter t.editor (fun i person ->
    let subwin = make_person_subdiv t i person in
    Dom.appendChild t.persons_area (Html.createBr (Page.document t.page));
    Dom.appendChild t.persons_area subwin;
    Lwt.return ())
  |> ignore

let make_search_result t score =
  let person = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Person.slug person in
  let%lwt name = Person.name person in
  let row = Table.Row.create
    ~on_click:(fun () ->
      Table.set_visible t.search_results false;
      Inputs.Text.erase t.search_bar;
      Lwt.on_success 
        (CreditEditor.add t.editor (`Slug slug)) 
        (fun () -> refresh t))
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) t.page;
      Table.Cell.text ~text:(Lwt.return name) t.page]
    t.page
  in
  Lwt.return row

let make_default_result t = 
  Table.Row.create
    ~on_click:(fun () ->
      Table.set_visible t.search_results false;
      Inputs.Text.erase t.search_bar;
      Lwt.on_success 
        (CreditEditor.add t.editor (`New "")) 
        (fun () -> refresh t))
    ~cells:[
      Table.Cell.text ~text:(Lwt.return "+") t.page;
      Table.Cell.text ~text:(Lwt.return "Create a new person") t.page]
    t.page
  |> Lwt.return

let create page =
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
  let search_results = Table.create
    ~kind:Table.Kind.Dropdown
    ~visible:false
    page
  in
  let search_bar = Inputs.Text.create
    ~default:"Search for a person"
    ~on_focus:(fun b -> if b then Table.set_visible search_results true)
    page
  in
  let t = {
    page; editor; content; persons_area; search_bar; search_results; input_name;
    persons_inputs = []} 
  in
  let rec is_child_of c p =
    ((c :> Dom.node Js.t) = (p :> Dom.node Js.t)) ||
    (Js.Opt.case c##.parentNode
      (fun () -> false)
      (fun p' -> is_child_of p' p))
  in
  Inputs.Text.on_change search_bar (fun txt ->
    if String.length txt > 2 then begin
      let open Lwt in
(*       let persons = Person.search ~threshold:0.6 [txt] in *)
      let persons = Lwt.return [] in
      Lwt.on_success persons (fun scores ->
        let def_result = make_default_result t in
        NesList.sub 10 scores
        |> Lwt_list.map_p (make_search_result t)
        >>= (fun l -> let%lwt r = def_result in Lwt.return (l@[r]))
        |> Table.replace_rows search_results)
    end);
  Lwt.async (fun () -> Lwt_js_events.clicks (Page.document page)
    (fun ev _ ->
      Js.Opt.case ev##.target
        (fun () -> ())
        (fun trg ->
          if not (is_child_of (trg :> Dom.node Js.t) (Table.root search_results :> Dom.node Js.t))
          && not (is_child_of (trg :> Dom.node Js.t) (Inputs.Text.root search_bar :> Dom.node Js.t)) then
            Table.set_visible search_results false);
      Lwt.return ()));
  let submit = Html.createDiv (Page.document page) in
  Style.set ~display:"flex" submit;
  submit##.classList##add (js "justify-content-space-between");
  let save =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Success ~icon:"save" ~text:"Save"
      ~on_click:(fun () ->
        let b1, b2, b3 =
          Inputs.Text.check t.input_name (fun str -> str <> ""),
          Inputs.Text.check t.search_bar (fun _ -> CreditEditor.count t.editor > 0),
          List.for_all (fun input -> Inputs.Text.check input (fun str -> str <> ""))
            t.persons_inputs
        in
        if b1 && b2 && b3 then (
          Lwt.on_success (CreditEditor.submit editor) (fun credit ->
          Lwt.on_success (Credit.slug credit) (fun slug ->
          let href = Router.path_of_controller (Router.Credit slug) |> snd in
          Html.window##.location##.href := js href))))
      page
  in
  let clear =
    Inputs.Button.create ~kind:Inputs.Button.Kind.Danger ~icon:"exclamation-triangle" ~text:"Clear"
      ~on_click:(fun () ->
        if Html.window##confirm (js "Clear the editor?") |> Js.to_bool then begin
          CreditEditor.clear editor;
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
  Dom.appendChild form persons_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root search_bar);
  Dom.appendChild form (Table.root search_results);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content form;
  t

let contents t =
  t.content
