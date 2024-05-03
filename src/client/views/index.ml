open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_components
open Dancelor_client_utils
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Page = Dancelor_client_page
open Dancelor_client_html

let search slice input =
  let threshold = 0.4 in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in (* FIXME: AnyFilter.from_string should return a result lwt *)
  let%lwt results = Any.search ~threshold ~slice filter in
  Lwt.return_ok results

let create () =
  Page.make ~title:(S.const "") @@
  div [

    QuickSearchBar.make_and_render
      ~placeholder:"Search for anything (it's magic!)"
      ~search
      ~make_result:(Lwt.return % AnyResultNewAPI.make_result)
      ~autofocus:true
      ~on_enter:(fun search_text ->
          Dom_html.window##.location##.href := Js.string PageRouter.(path_explore (Some search_text))
        )
      ()
  ]
