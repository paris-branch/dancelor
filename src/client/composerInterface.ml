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
  tunes_area : Html.divElement Js.t;
  input_name : Inputs.Text.t;
  input_kind : Inputs.Text.t;
}

let rec make_tune_subwindow t index tune =
  let subwin = Html.createDiv (Page.document t.page) in
  subwin##.classList##add (js "subwindow");
  let toolbar = Html.createDiv (Page.document t.page) in
  toolbar##.classList##add (js "toolbar");
  let title = Text.Heading.h3 ~text:(TuneGroup.name tune.Composer.group) t.page in
  Dom.appendChild toolbar (Text.Heading.root title);
  let buttons = Html.createUl (Page.document t.page) in
  let down, up, del = 
    Inputs.Button.create 
      ~on_click:(fun () -> 
        Composer.move_down t.composer index; 
        Composer.save t.composer;
        refresh t)
      ~text:"v" t.page, 
    Inputs.Button.create 
      ~on_click:(fun () -> 
        Composer.move_up t.composer index; 
        Composer.save t.composer;
        refresh t)
      ~text:"^" t.page, 
    Inputs.Button.create 
      ~on_click:(fun () -> 
        Composer.remove t.composer index; 
        Composer.save t.composer;
        refresh t)
      ~kind:Inputs.Button.Kind.Danger
      ~text:"x" t.page
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
    Printf.sprintf "/%s%s" 
      Constant.api_prefix 
      (Router.path_of_controller (Router.TunePng tune.Composer.slug) |> snd) 
    |> Lwt.return
  in
  let img = Image.create ~source t.page in
  Dom.appendChild subwin (Image.root img);
  subwin

and refresh t = 
  Inputs.Text.set_contents t.input_name (Composer.name t.composer);
  Inputs.Text.set_contents t.input_kind (Composer.kind t.composer);
  while Js.to_bool t.tunes_area##hasChildNodes do
    let child = Js.Opt.get t.tunes_area##.firstChild (fun () -> assert false) in
    t.tunes_area##removeChild child |> ignore
  done;
  Composer.iter t.composer (fun i tune -> 
    let subwin = make_tune_subwindow t i tune in
    Dom.appendChild t.tunes_area (Html.createBr (Page.document t.page));
    Dom.appendChild t.tunes_area subwin)

let create page = 
  let composer = Composer.create () in
  let content = Html.createDiv (Page.document page) in
  let title = Text.Heading.h1 ~text:(Lwt.return "Compose a Set") page in
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
  let tunes_area = Html.createDiv (Page.document page) in
  let search_results = Table.create
    ~kind:Table.Kind.Dropdown
    ~visible:false
    page
  in
  let search_bar = Inputs.Text.create
    ~default:"Search for a tune"
    ~on_change:(fun _ -> ())
    ~on_focus:(fun b -> Table.set_visible search_results b)
    page
  in
  let t = {page; composer; content; tunes_area; input_name; input_kind} in
  let test_button = Inputs.Button.create
    ~on_click:(fun () -> 
      let tune_slug = Inputs.Text.contents search_bar in
      Lwt.on_success 
        (Composer.add composer tune_slug)
        (fun () -> refresh t))
    ~text:"Test"
    page
  in
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr (Page.document page));
  Dom.appendChild content (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_name);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root input_kind);
  Dom.appendChild form tunes_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root search_bar);
  Dom.appendChild form (Table.root search_results);
  Dom.appendChild form (Inputs.Button.root test_button);
  Dom.appendChild content form;
  Lwt.on_success (Composer.load composer) (fun () -> refresh t);
  t

let contents t = 
  t.content
