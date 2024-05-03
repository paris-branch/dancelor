open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_components
open Dancelor_client_utils
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Page = Dancelor_client_page

let js = Js.string

type t =
  {
    page : Page.t;
    document : Dom_html.document Js.t;
    content : Dom_html.divElement Js.t;
  }

let search slice input =
  let threshold = 0.4 in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in (* FIXME: AnyFilter.from_string should return a result lwt *)
  let%lwt results = Any.search ~threshold ~slice filter in
  Lwt.return_ok results

let create page =
  let document = Dom_html.window##.document in
  let content = Dom_html.createDiv document in

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [

      QuickSearchBar.make_and_render
        ~placeholder:"Search for anything (it's magic!)"
        ~search
        ~make_result:(Lwt.return % AnyResultNewAPI.make_result)
        ~autofocus:true
        ~on_enter:(fun search_text ->
            Dom_html.window##.location##.href := js PageRouter.(path_explore (Some search_text))
          )
        ()
    ]
  );

  {page; document; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
