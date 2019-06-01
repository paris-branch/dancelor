open Js_of_ocaml
open Js_of_ocaml_lwt
open Dancelor_client_model
open Dancelor_client_elements
open Dancelor_common

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
      ~icon:"chevron-down"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        Composer.move_up t.composer index;
        Composer.save t.composer;
        refresh t)
      ~icon:"chevron-up"
      t.page,
    Inputs.Button.create
      ~on_click:(fun () ->
        Composer.remove t.composer index;
        Composer.save t.composer;
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

let make_search_result t score =
  let tune = Score.value score in
  let score = score.Score.score in
  let%lwt slug = Tune.slug tune in
  let%lwt bars = Tune.bars tune in
  let%lwt structure = Tune.structure tune in
  let%lwt group = Tune.group tune in
  let%lwt name = TuneGroup.name group in
  let%lwt kind = TuneGroup.kind group in
  let row = Table.Row.create
    ~on_click:(fun () ->
      Table.set_visible t.search_results false;
      Inputs.Text.erase t.search_bar;
      Lwt.on_success (Composer.add t.composer slug) (fun () -> refresh t; Composer.save t.composer))
    ~cells:[
      Table.Cell.text ~text:(Lwt.return (string_of_int (int_of_float (score *. 100.)))) t.page;
      Table.Cell.text ~text:(Lwt.return name) t.page;
      Table.Cell.text ~text:(Lwt.return (string_of_int bars)) t.page;
      Table.Cell.text ~text:(Lwt.return (Kind.base_to_string kind)) t.page;
      Table.Cell.text ~text:(Lwt.return structure) t.page]
    t.page
  in
  Lwt.return row

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
    ~on_focus:(fun b -> if b then Table.set_visible search_results true)
    page
  in
  let t = {page; composer; content; tunes_area; search_bar; search_results; input_name; input_kind} in
  let rec is_child_of c p =
    ((c :> Dom.node Js.t) = (p :> Dom.node Js.t)) ||
    (Js.Opt.case c##.parentNode
      (fun () -> false)
      (fun p' -> is_child_of p' p))
  in
  Inputs.Text.on_change search_bar (fun txt ->
    if String.length txt > 2 then begin
      let tunes = Tune.search ~threshold:0.6 [txt] in
      Lwt.on_success tunes (fun scores ->
        NesList.sub 10 scores
        |> Lwt_list.map_p (make_search_result t)
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
          Inputs.Text.check t.input_kind Kind.check_dance,
          Inputs.Text.check t.input_name (fun str -> str <> ""),
          Inputs.Text.check t.search_bar (fun _ -> Composer.count t.composer > 0)
        in
        if b1 && b2 && b3 then (
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
          Composer.save composer;
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
  Dom.appendChild form tunes_area;
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form (Inputs.Text.root search_bar);
  Dom.appendChild form (Table.root search_results);
  Dom.appendChild form (Html.createBr (Page.document page));
  Dom.appendChild form submit;
  Dom.appendChild content form;
  Lwt.on_success (Composer.load composer) (fun () -> refresh t);
  t

let contents t =
  t.content
