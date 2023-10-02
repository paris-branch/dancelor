open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_utils
open Dancelor_client_model
open Dancelor_client_html
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    document : Html.document Js.t;
    content : Html.divElement Js.t;
  }

let search input =
  let threshold = 0.4 in
  let pagination = Pagination.{ start = 0; end_ = 15 } in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in (* FIXME: AnyFilter.from_string should return a result lwt *)
  let%lwt results = Any.search ~threshold ~pagination filter in
  Lwt.return_ok results

(** Row for when the search returned no results. *)
let no_results_row =
  tr [
    td [txt "⚠️"];
    td [txt "Your search returned no results."];
  ]

(** Rows for when the search returned error messages. *)
let error_rows messages =
  flip List.map messages @@ fun message ->
  tr [
    td [txt "❌"];
    td [txt message];
  ]

let create page =
  let document = Html.window##.document in
  let content = Html.createDiv document in

  let (search_text, set_search_text) = S.create "" in
  let (table_visible, set_table_visible) = S.create false in

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [

      div [
        input ~a:[
          a_input_type `Text;
          a_placeholder "Search for anything (it's magic!)";
          a_oninput (fun event ->
              (
                Js.Opt.iter event##.target @@ fun elt ->
                Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                set_search_text (Js.to_string input##.value)
              );
              false
            );
          a_onfocus (fun _ -> set_table_visible true; false);
          a_onblur (fun _ -> set_table_visible false; false);
          a_onkeyup (fun event ->
              if Js.Optdef.to_option event##.key = Some (js "Enter") then
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  let search_text = Js.to_string input##.value in
                  Dom_html.window##.location##.href := js PageRouter.(path (Search (Some search_text)))
                );
              true
            );
          (* FIXME: make focused at the beginning. *)
        ] ();

        tablex
          ~a:[
            R.a_class (
              flip S.map table_visible @@ function
              | false -> ["dropdown-table"]
              | true -> ["dropdown-table"; "visible"]
            );
          ]
          [
            R.tbody (
              S.bind search_text @@ fun search_text ->
              S.from' [] @@
              if String.length search_text < 3 then
                (
                  let message =
                    if search_text = ""
                    then "Start typing to search."
                    else "Type at least three characters."
                  in
                  Lwt.return [
                    tr [
                      td [txt "👉"];
                      td [txt message];
                    ]
                  ]
                )
              else
                flip Lwt.map (search search_text) @@ function
                | Error messages -> error_rows messages
                | Ok [] -> [no_results_row]
                | Ok results ->
                  List.map AnyResultNewAPI.make_result (List.sub 10 results)
                  @ [tr [td [txt "👉"]; td ~a:[a_colspan 4] [txt "Press enter for more results."]]]
            );
          ]
      ]
    ]
  );

  {page; document; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
