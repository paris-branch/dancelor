open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  document : Html.document Js.t;
  content : Html.divElement Js.t;
  search : SearchBar.t;
}

let search input =
  let threshold = 0.4 in
  let pagination = Pagination.{ start = 0; end_ = 15 } in
  match AnyFilter.from_string input with
  | Ok filter ->
    let%lwt results = Any.search ~threshold ~pagination filter in
    Lwt.return_ok results
  | Error err -> Lwt.return_error err

let create page =
  let document = Html.window##.document in
  let content = Html.createDiv document in

  let search =
    let main_section =
      SearchBar.Section.create
        ~search ~make_result:(AnyResult.make_result page)
        page
    in
    SearchBar.create
      ~on_enter:(fun input ->
          Dom_html.window##.location##.href := js (spf "/search?q=%s" input);
          Lwt.return_unit)
      ~placeholder:"Search for anything (it's magic!)"
      ~sections:[main_section]
      page
  in

  Dom.appendChild content (SearchBar.root search);
  {page; document; content; search}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  SearchBar.focus t.search
