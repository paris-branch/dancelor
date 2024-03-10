open Nes
open Js_of_ocaml
open Dancelor_client_model
open Dancelor_client_components
module Elements = Dancelor_client_elements
module Formatters = Dancelor_client_formatters
module Utils = Dancelor_client_utils
module PageRouter = Dancelor_common_pageRouter

let js = Js.string

type t =
  {
    page : Elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let update_uri input =
  let uri = PageRouter.path_explore (Some input) in
  Dom_html.window##.history##replaceState
    "fixme-the-state" (js "") (Js.some (js uri))

let search ?slice input =
  let threshold = 0.4 in
  let%rlwt filter = Lwt.return (Any.Filter.from_string input) in
  Lwt.map Result.ok @@ Any.search ~threshold ?slice filter

(** Generic row showing an emoji on the left and a message on the right. *)
let emoji_row emoji message =
  let open Dancelor_client_html in
  tr [
    td [txt emoji];
    td ~a:[a_colspan 4] [txt message];
  ]

let create ?query page =
  let document = Elements.Page.document page in
  let content = Dom_html.createDiv document in

  document##.title := js ("Explore | Dancelor");

  let open Dancelor_client_html in

  let (number_of_entries, set_number_of_entries) = S.create None in

  let pagination =
    Pagination.create
      ~entries_per_page: 25
      ~number_of_entries
  in

  let search_bar =
    SearchBar.make
      ~search: (fun slice input -> search ~slice input)
      ~slice: (Pagination.slice pagination)
      ~on_number_of_entries: (set_number_of_entries % Option.some)
      ?initial_input: query
      ()
  in

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "Explore"];

      SearchBar.render
        ~placeholder:"Search for anything (it really is magic!)"
        ~autofocus:true
        ~on_input:update_uri
        search_bar;

      div ~a:[a_class ["buttons"]] [
        a
          ~a:[
            a_class ["button"];
            a_onclick (fun _ ->
                Lwt.async (fun () ->
                    let search_text = S.value (SearchBar.text search_bar) in
                    (* TODO: On return, add a space and focus the search bar. *)
                    Fun.flip Lwt.map
                      (SearchComplexFiltersDialog.open_ search_text)
                      (Result.iter (fun text -> SearchBar.set_text search_bar text; update_uri text))
                  );
                false);
          ]
          [
            i ~a:[a_class ["fas"; "fa-filter"]] [];
            txt " Complex filter";
          ]
      ];

      R.div (
        Fun.flip S.map (SearchBar.state search_bar) @@ function
        | NoResults -> [div ~a:[a_class ["warning"]] [txt "Your search returned no results."]]
        | Errors error -> [div ~a:[a_class ["error"]] [txt error]]
        | StartTyping | ContinueTyping | Results _ -> []
      );

      div
        ~a:[
          R.a_class (
            Fun.flip S.map (SearchBar.state search_bar) @@ function
            | Results _ -> []
            | _ -> ["hidden"]
          )
        ]
        [
          Pagination.render pagination;

          tablex
            ~a:[a_class ["separated-table"]]
            ~thead:(
              thead [
                tr [th [txt "Type"]; th [txt "Name"]; th [txt "Kind"]; th [txt "By"];]
              ];
            )
            [
              R.tbody (
                Fun.flip S.map (S.Pair.pair (Pagination.slice pagination) (SearchBar.state search_bar))
                @@ fun (_, state) ->
                match state with
                | Results results ->
                  let context = S.map PageRouter.inSearch @@ SearchBar.text search_bar in
                  List.map Utils.AnyResultNewAPI.(make_result ~context) results
                | _ -> []
              )
            ];

          Pagination.render pagination;
        ]
    ]
  );

  { page; content }

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
