open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type filter =
  { tune : Tune.t Slug.t list ;
    tune_author : Credit.t Slug.t list ;
    tune_kind : Kind.base list ;
    key : Music.key list ;
    bars : int list }

let filter_empty = { tune = []; tune_author = []; tune_kind = []; key = []; bars = [] }

let filter_add_key k t =
  {t with key = k :: t.key}

let filter_remove_key k t =
  {t with key = List.filter ((<>) k) t.key}

let filter_add_bars b t =
  {t with bars = b :: t.bars}

let filter_remove_bars b t =
  {t with bars = List.filter ((<>) b) t.bars}

let filter_add_kind k t =
  {t with tune_kind = k :: t.tune_kind}

let filter_remove_kind k t =
  {t with tune_kind = List.filter ((<>) k) t.tune_kind}

let filter_to_versionfilter filter =
  let%lwt tunes = Lwt_list.map_p Tune.get filter.tune in
  let%lwt tune_authors = Lwt_list.map_p Credit.get filter.tune_author in

  let or_if_not_empty = function
    | [] -> Formula.true_
    | l -> Formula.or_l l
  in

  let tunes =
    tunes
    |> List.map VersionFilter.tuneIs
    |> or_if_not_empty
  in

  let tune_authors =
    tune_authors
    |> List.map (fun author -> VersionFilter.tune (TuneFilter.authorIs author))
    |> or_if_not_empty
  in

  let tune_kinds =
    filter.tune_kind
    |> List.map (fun kind -> VersionFilter.tune (TuneFilter.kind kind))
    |> or_if_not_empty
  in

  let key =
    filter.key
    |> List.map (fun key -> VersionFilter.key key)
    |> or_if_not_empty
  in

  let bars =
    filter.bars
    |> List.map (fun bars -> VersionFilter.kind (KindFilter.Version.barsEq bars))
    |> or_if_not_empty
  in

  [tunes; tune_authors; tune_kinds; key; bars]
  |> Formula.and_l
  |> Lwt.return

type t =
{
  page : Page.t;
  content : Html.divElement Js.t;
  page_nav : PageNav.t;
  search_div : Html.divElement Js.t;
  mutable search : filter;
  table : Table.t;
}

let major_keys = Music.[
    make_pitch C Natural 0 ;
    make_pitch G Natural 0 ;
    make_pitch D Natural 0 ;
    make_pitch A Natural 0 ;
    make_pitch E Natural 0 ;
    make_pitch B Natural 0 ;
    make_pitch F  Sharp  0 ;
    make_pitch C  Sharp  0 ;
    make_pitch F Natural 0 ;
    make_pitch B   Flat  0 ;
    make_pitch E   Flat  0 ;
    make_pitch A   Flat  0 ;
    make_pitch D   Flat  0 ;
  ]

let minor_keys = Music.[
    make_pitch A Natural 0 ;
    make_pitch E Natural 0 ;
    make_pitch B Natural 0 ;
    make_pitch F  Sharp  0 ;
    make_pitch C  Sharp  0 ;
    make_pitch G  Sharp  0 ;
    make_pitch D  Sharp  0 ;
    make_pitch A  Sharp  0 ;
    make_pitch D Natural 0 ;
    make_pitch G Natural 0 ;
    make_pitch C Natural 0 ;
    make_pitch F Natural 0 ;
    make_pitch B   Flat  0 ;
  ]

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
    let%lwt versions =
      let filter = t.search in
      let%lwt filter = filter_to_versionfilter filter in
      let pagination = PageNav.pagination t.page_nav in
      Version.search ~pagination filter
      >|=| Score.list_erase
    in
    Lwt.return (List.map (fun version ->
        let href =
          let%lwt slug = Version.slug version in
          Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
        in
        let cells =
          let tune = Version.tune version in
          let open Lwt in [
            Table.Cell.create ~content:(
              let%lwt content = Formatters.Version.name_and_disambiguation version in
              Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document t.page) content)
            ) t.page;
            Table.Cell.create ~content:(
              let%lwt tune = tune in
              let%lwt content = Formatters.Kind.full_string version tune in
              Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document t.page) content)
            ) t.page;
            Table.Cell.text ~text:(Version.key version >|= Music.key_to_pretty_string) t.page;
            Table.Cell.text ~text:(Version.structure version) t.page;
            Table.Cell.create ~content:(
              let%lwt content = Formatters.Version.author_and_arranger version in
              Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document t.page) content)
            ) t.page; ]
        in
        Table.Row.create ~href ~cells t.page) versions)
  in
  let section = Table.Section.create ~rows t.page in
  Table.replace_bodies t.table (Lwt.return [section])

let update_filter t upd =
  t.search <- upd t.search;
  Lwt.on_success (filter_to_versionfilter t.search) @@ fun filter ->
  Lwt.on_success (Version.count filter) (PageNav.set_entries t.page_nav)

let fill_search t =
  let document = Page.document t.page in
  let key_section_header = Html.createB document in
  key_section_header##.textContent := Js.some (js "Filter by key:");
  Dom.appendChild t.search_div key_section_header;
  let line1 = Html.createDiv document in
  List.iter (fun key ->
    let key = Music.make_key key Music.Major in
    let text = Music.key_to_pretty_string key in
    let id = spf "button_%s" (Music.key_to_safe_string key) in
    let on_change active =
      if active then update_filter t (filter_add_key key)
      else update_filter t (filter_remove_key key)
    in
    let b = Inputs.Toggle.create ~id ~text ~on_change t.page in
    Style.set ~width:"3rem" ~margin:"0pt 2pt 2pt 0pt" (Inputs.Toggle.root b);
    Dom.appendChild line1 (Inputs.Toggle.root b))
    major_keys;
  Dom.appendChild t.search_div line1;
  let line2 = Html.createDiv document in
  List.iter (fun key ->
    let key = Music.make_key key Music.Minor in
    let text = Music.key_to_pretty_string key in
    let id = spf "button_%s" (Music.key_to_safe_string key) in
    let on_change active =
      if active then update_filter t (filter_add_key key)
      else update_filter t (filter_remove_key key)
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
    let text = Kind.base_to_pretty_string kind in (** FIXME: really, a pretty string in an id? *)
    let id = spf "button_%s" text in
    let on_change active =
      if active then update_filter t (filter_add_kind kind)
      else update_filter t (filter_remove_kind kind)
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
    let text = spf "%d Bars" n_bars in
    let id = spf "button_%dbars" n_bars in
    let on_change active =
      if active then update_filter t (filter_add_bars n_bars)
      else update_filter t (filter_remove_bars n_bars)
    in
    let b = Inputs.Toggle.create ~id ~text ~on_change t.page in
    Style.set ~width:"6rem" ~margin:"0pt 2pt 2pt 0pt" (Inputs.Toggle.root b);
    Dom.appendChild bars (Inputs.Toggle.root b))
    lengths;
  Dom.appendChild t.search_div bars

let create page =
  let document = Page.document page in

  document##.title := js "All Tunes";

  let content = Html.createDiv document in
  let title = Text.Heading.h2_static ~text:(Lwt.return "All Tunes") page in
  Dom.appendChild content (Text.Heading.root title);
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let search_div = Html.createDiv document in
  Dom.appendChild content search_div;
  let search = filter_empty in
  Dom.appendChild content (Html.createBr document);
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~alt:(Lwt.return "Tunes") ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Key") page;
        Table.Cell.header_text ~text:(Lwt.return "Structure") page;
        Table.Cell.header_text ~text:(Lwt.return "Author") page;
      ]
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
  Lwt.on_success (Version.count Formula.true_) (fun entries ->
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
