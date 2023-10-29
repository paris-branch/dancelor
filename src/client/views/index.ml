open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_components
open Dancelor_client_utils
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    document : Html.document Js.t;
    content : Html.divElement Js.t;
  }

let search input =
  let threshold = 0.4 in
  let pagination = Pagination.{ start = 0; end_ = 15 } in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in (* FIXME: AnyFilter.from_string should return a result lwt *)
  let%lwt results = Any.search ~threshold ~pagination filter in
  Lwt.return_ok results

let create page =
  let document = Html.window##.document in
  let content = Html.createDiv document in

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [

      SearchBar.make
        ~placeholder:"Search for anything (it's magic!)"
        ~search
        ~make_result:AnyResultNewAPI.make_result
        ~max_results:10
        ~on_enter:(fun search_text ->
            Dom_html.window##.location##.href := js PageRouter.(path (Search (Some search_text)))
          )
    ]
  );

  {page; document; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
