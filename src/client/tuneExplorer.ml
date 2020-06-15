open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_common
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  content : Html.divElement Js.t;
  page_nav : PageNav.t;
  search_div : Html.divElement Js.t;
  mutable search : TuneFilter.t Lwt.t;
  table : Table.t;
}

let major_keys = Music.[
  (C, Natural);
  (G, Natural);
  (D, Natural);
  (A, Natural);
  (E, Natural);
  (B, Natural);
  (F, Sharp);
  (C, Sharp);
  (F, Natural);
  (B, Flat);
  (E, Flat);
  (A, Flat);
  (D, Flat)]

let minor_keys = Music.[
  (A, Natural);
  (E, Natural);
  (B, Natural);
  (F, Sharp);
  (C, Sharp);
  (G, Sharp);
  (D, Sharp);
  (A, Sharp);
  (D, Natural);
  (G, Natural);
  (C, Natural);
  (F, Natural);
  (B, Flat)]

let kinds = Kind.[
  Reel;
  Strathspey;
  Jig;
  Waltz]

let lengths = [
  32;
  40;
  48;
  64]

let update_table t = 
  let rows = 
    let%lwt tunes =
      let%lwt filter = t.search in
      let pagination = PageNav.pagination t.page_nav in
      Tune.all ~filter ~pagination ()
    in
    Lwt.return (List.map (fun tune ->
      let href =
        let%lwt slug = Tune.slug tune in
        Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
      in
      let cells =
        let group = Tune.group tune in
        let open Lwt in [
        Table.Cell.link ~href ~text:(group >>= TuneGroup.name) t.page;
        Table.Cell.text ~text:(group >>= Formatters.Kind.full_string tune) t.page;
        Table.Cell.text ~text:(Tune.key tune >|= Music.key_to_string) t.page;
        Table.Cell.text ~text:(Tune.structure tune) t.page;
        Table.Cell.text ~text:(group >>= TuneGroup.author >>= Formatters.Credit.line) t.page]
      in
      Table.Row.create ~href ~cells t.page) tunes)
  in
  let section = Table.Section.create ~rows t.page in
  Table.replace_bodies t.table (Lwt.return [section])

let update_filter t upd = 
  t.search <- Lwt.bind t.search upd;
  Lwt.on_success t.search (fun filter ->
    Lwt.on_success (Tune.count ~filter ()) (PageNav.set_entries t.page_nav))

let fill_search t = 
  let document = Page.document t.page in
  let key_section_header = Html.createB document in
  key_section_header##.textContent := Js.some (js "Filter by key:");
  Dom.appendChild t.search_div key_section_header;
  let line1 = Html.createDiv document in
  List.iter (fun key ->
    let key = (key, Music.Major) in
    let text = Music.pprint_key key in
    let id = Printf.sprintf "button_%s" (Music.key_to_slug key) in
    let on_change active =
      if active then update_filter t (TuneFilter.add_key key)
      else update_filter t (TuneFilter.remove_key key)
    in
    let b = Inputs.Toggle.create ~id ~text ~on_change t.page in
    Style.set ~width:"3rem" ~margin:"0pt 2pt 2pt 0pt" (Inputs.Toggle.root b);
    Dom.appendChild line1 (Inputs.Toggle.root b))
    major_keys;
  Dom.appendChild t.search_div line1;
  let line2 = Html.createDiv document in
  List.iter (fun key ->
    let key = (key, Music.Minor) in
    let text = Music.pprint_key key in
    let id = Printf.sprintf "button_%s" (Music.key_to_slug key) in
    let on_change active =
      if active then update_filter t (TuneFilter.add_key key)
      else update_filter t (TuneFilter.remove_key key)
    in
    let b = Inputs.Toggle.create ~id ~text ~on_change t.page in
    Style.set ~width:"3rem" ~margin:"0pt 2pt 2pt 0pt" (Inputs.Toggle.root b);
    Dom.appendChild line2 (Inputs.Toggle.root b))
    minor_keys;
  Dom.appendChild t.search_div line2;
  Dom.appendChild t.search_div (Html.createBr document);
  let kind_section_header = Html.createB document in
  kind_section_header##.textContent := Js.some (js "Filter by kind:");
  Dom.appendChild t.search_div kind_section_header;
  let kinds_div = Html.createDiv document in
  List.iter (fun kind ->
    let text = Kind.pprint_base kind in
    let id = Printf.sprintf "button_%s" text in
    let on_change active =
      if active then update_filter t (TuneFilter.add_kind kind)
      else update_filter t (TuneFilter.remove_kind kind)
    in
    let b = Inputs.Toggle.create ~id ~text ~on_change t.page in
    Style.set ~width:"6rem" ~margin:"0pt 2pt 2pt 0pt" (Inputs.Toggle.root b);
    Dom.appendChild kinds_div (Inputs.Toggle.root b))
    kinds;
  Dom.appendChild t.search_div kinds_div;
  Dom.appendChild t.search_div (Html.createBr document);
  let bars_section_header = Html.createB document in
  bars_section_header##.textContent := Js.some (js "Filter by length:");
  Dom.appendChild t.search_div bars_section_header;
  let bars = Html.createDiv document in
  List.iter (fun n_bars ->
    let text = Printf.sprintf "%d Bars" n_bars in
    let id = Printf.sprintf "button_%dbars" n_bars in
    let on_change active =
      if active then update_filter t (TuneFilter.add_bars n_bars)
      else update_filter t (TuneFilter.remove_bars n_bars)
    in
    let b = Inputs.Toggle.create ~id ~text ~on_change t.page in
    Style.set ~width:"6rem" ~margin:"0pt 2pt 2pt 0pt" (Inputs.Toggle.root b);
    Dom.appendChild bars (Inputs.Toggle.root b))
    lengths;
  Dom.appendChild t.search_div bars

let create page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let title = Text.Heading.h1 ~text:(Lwt.return "All Tunes") page in
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let search_div = Html.createDiv document in
  Dom.appendChild content search_div;
  let search = TuneFilter.make () in
  Dom.appendChild content (Html.createBr document);
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Tunes") ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Key") page;
        Table.Cell.header_text ~text:(Lwt.return "Structure") page;
        Table.Cell.header_text ~width:"30%" ~text:(Lwt.return "Author") page]
      page
  in
  let table = Table.create ~kind:Table.Kind.Separated ~header page in
  Dom.appendChild content (Table.root table);
  let page_nav = PageNav.create ~entries:0 ~entries_per_page:25 page in
  Dom.appendChild content (PageNav.root page_nav);
  let t = {page; content; search_div; search; table; page_nav} in
  PageNav.connect_on_page_change page_nav (fun _ ->
    PageNav.rebuild page_nav;
    update_table t);
  Lwt.on_success (Tune.count ()) (fun entries ->
    PageNav.set_entries page_nav entries);
  fill_search t;
  update_table t;
  t

let contents t =
  t.content

let refresh t = 
  ignore t

let init t = 
  ignore t
